////
//// NOTE: Don't forget to update the moduledoc of gleam/otp/actor if you
//// change this file.
////

import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/function
import gleam/int
import gleam/otp/actor
import gleam/otp/system
import gleam/result
import gleeunit/should

pub fn get_state_test() {
  let assert Ok(actor) =
    actor.new("Test state")
    |> actor.on_message(fn(_msg, state) { actor.continue(state) })
    |> actor.start

  actor.pid
  |> system.get_state
  |> should.equal(dynamic.from("Test state"))
}

@external(erlang, "sys", "get_status")
fn get_status(a: Pid) -> Dynamic

pub fn get_status_test() {
  let assert Ok(actor) =
    actor.new(Nil)
    |> actor.on_message(fn(_msg, state) { actor.continue(state) })
    |> actor.start

  actor.pid
  |> get_status
  // TODO: assert something about the response
}

pub fn failed_init_test() {
  actor.new_with_initialiser(100, fn() { Error("not enough wiggles") })
  |> actor.on_message(fn(state, _msg) { actor.continue(state) })
  |> actor.start
  |> result.is_error
  |> should.be_true
}

pub fn timed_out_init_test() {
  process.trap_exits(True)
  let exit_selector =
    process.new_selector()
    |> process.selecting_trapped_exits(function.identity)

  let result =
    actor.new_with_initialiser(1, fn() {
      process.sleep(100)
      panic as "should not be reached"
    })
    |> actor.on_message(fn(_state, _msg) { panic as "should not be reached" })
    |> actor.start

  // Check that the exit isn't unhandled: it should be handled by start.
  // Stop trapping exits before asserting, to avoid interfering with other tests.
  let exit = process.select(exit_selector, 10)
  process.trap_exits(False)

  result |> should.equal(Error(actor.InitTimeout))
  exit |> should.equal(Error(Nil))
}

pub fn suspend_resume_test() {
  let assert Ok(actor) =
    actor.new(0)
    |> actor.on_message(fn(iter, _msg) { actor.continue(iter + 1) })
    |> actor.start

  // Suspend process
  actor.pid
  |> system.suspend
  |> should.equal(Nil)

  // This normal message will not be handled yet so the state remains 0
  actor.send(actor.data, "hi")

  // System messages are still handled
  actor.pid
  |> system.get_state
  |> should.equal(dynamic.from(0))

  // Resume process
  actor.pid
  |> system.resume
  |> should.equal(Nil)

  // The queued regular message has been handled so the state has incremented
  actor.pid
  |> system.get_state
  |> should.equal(dynamic.from(1))
}

pub fn subject_test() {
  let assert Ok(actor) =
    actor.new("state 1")
    |> actor.on_message(fn(_state, msg) { actor.continue(msg) })
    |> actor.start

  actor.pid
  |> system.get_state()
  |> should.equal(dynamic.from("state 1"))

  actor.send(actor.data, "state 2")

  actor.pid
  |> system.get_state()
  |> should.equal(dynamic.from("state 2"))
}

pub fn unexpected_message_test() {
  // Quieten the logger
  logger_set_primary_config(atom.create("level"), atom.create("error"))

  let assert Ok(actor) =
    actor.new("state 1")
    |> actor.on_message(fn(_state, msg) { actor.continue(msg) })
    |> actor.start

  actor.pid
  |> system.get_state()
  |> should.equal(dynamic.from("state 1"))

  raw_send(actor.pid, "Unexpected message 1")
  actor.send(actor.data, "state 2")
  raw_send(actor.pid, "Unexpected message 2")

  actor.pid
  |> system.get_state()
  |> should.equal(dynamic.from("state 2"))
}

pub fn unexpected_message_handled_test() {
  let assert Ok(actor) =
    actor.new_with_initialiser(10, fn() {
      let selector =
        process.new_selector() |> process.selecting_anything(function.identity)
      actor.initialised(dynamic.from("initial"))
      |> actor.selecting(selector)
      |> Ok
    })
    |> actor.on_message(fn(_state, msg) { actor.continue(msg) })
    |> actor.start

  raw_send(actor.pid, "Unexpected message 1")

  actor.pid
  |> system.get_state()
  |> should.equal(dynamic.from("Unexpected message 1"))
}

type ActorMessage {
  UserMessage(String)
  Unknown(Dynamic)
  SetStringSelector(
    reply: Subject(Subject(String)),
    mapper: fn(String) -> ActorMessage,
  )
  SetIntSelector(reply: Subject(Subject(Int)), mapper: fn(Int) -> ActorMessage)
  GetText(reply: Subject(String))
}

pub fn replace_selector_test() {
  let assert Ok(actor) =
    actor.new_with_initialiser(50, fn() {
      let subject = process.new_subject()
      let selector =
        process.new_selector() |> process.selecting(subject, function.identity)
      actor.initialised(#(selector, "initial"))
      |> actor.returning(subject)
      |> actor.selecting(selector)
      |> Ok
    })
    |> actor.on_message(fn(state, msg) {
      case msg {
        UserMessage(string) ->
          actor.continue(#(state.0, "user message: " <> string))

        Unknown(val) ->
          actor.continue(#(
            state.0,
            "unknown message: " <> dynamic.classify(val),
          ))

        SetStringSelector(reply, mapper) -> {
          let #(subject, selector) = mapped_selector(state.0, mapper)
          process.send(reply, subject)
          actor.continue(state)
          |> actor.with_selector(selector)
        }

        SetIntSelector(reply, mapper) -> {
          let #(subject, selector) = mapped_selector(state.0, mapper)
          process.send(reply, subject)
          actor.continue(state)
          |> actor.with_selector(selector)
        }

        GetText(reply) -> {
          process.send(reply, state.1)
          actor.continue(state)
        }
      }
    })
    |> actor.start

  // Send initial user message to original subject
  process.send(actor.data, UserMessage("test 1"))
  // Check state
  process.call(actor.data, 50, GetText)
  |> should.equal("user message: test 1")

  // Get a new subject with string selector
  let str_subj =
    process.call(actor.data, 1000, SetStringSelector(_, UserMessage))
  // Send to new string subject
  process.send(str_subj, "test 2")
  // Check state
  process.call(actor.data, 50, GetText)
  |> should.equal("user message: test 2")

  // Get a new subject with int selector
  let int_subj =
    process.call(
      actor.data,
      1000,
      SetIntSelector(_, fn(n: Int) { UserMessage("test " <> int.to_string(n)) }),
    )
  // Send to new int subject
  process.send(int_subj, 3)
  // Check state
  process.call(actor.data, 50, GetText)
  |> should.equal("user message: test 3")

  // Try to send to old string subject
  process.send(str_subj, "test 4")
  // Check state
  process.call(actor.data, 50, GetText)
  |> should.equal("unknown message: Tuple of 2 elements")
}

pub fn abnormal_exit_can_be_trapped_test() {
  process.trap_exits(True)
  let exits =
    process.new_selector()
    |> process.selecting_trapped_exits(function.identity)

  // Make an actor exit with an abnormal reason
  let assert Ok(actor) =
    actor.new(Nil)
    |> actor.on_message(fn(_, _) { actor.continue(Nil) })
    |> actor.start
  process.send_abnormal_exit(actor.pid, "boo!")

  let trapped_reason = process.select(exits, 10)

  // Stop trapping exits, as otherwise other tests fail
  process.trap_exits(False)

  // The weird reason below is because of https://github.com/gleam-lang/erlang/issues/66
  trapped_reason
  |> should.equal(
    Ok(process.ExitMessage(actor.pid, process.Abnormal(dynamic.from("boo!")))),
  )
}

pub fn killed_exit_can_be_trapped_test() {
  process.trap_exits(True)
  let exits =
    process.new_selector()
    |> process.selecting_trapped_exits(function.identity)

  // Make an actor exit with a killed reason
  let assert Ok(actor) =
    actor.new(Nil)
    |> actor.on_message(fn(_, _) { actor.continue(Nil) })
    |> actor.start
  process.kill(actor.pid)

  let trapped_reason = process.select(exits, 10)

  // Stop trapping exits, as otherwise other tests fail
  process.trap_exits(False)

  trapped_reason
  |> should.equal(Ok(process.ExitMessage(actor.pid, process.Killed)))
}

fn mapped_selector(
  selector: process.Selector(ActorMessage),
  mapper: fn(a) -> ActorMessage,
) {
  let subject = process.new_subject()

  let selector =
    selector
    |> process.selecting(subject, mapper)
    // Always create a selector that catches unknown messages
    |> process.selecting_anything(Unknown)

  #(subject, selector)
}

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: anything) -> anything

@external(erlang, "logger", "set_primary_config")
fn logger_set_primary_config(a: Atom, b: Atom) -> Nil
