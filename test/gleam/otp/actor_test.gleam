import gleam/otp/actor.{Continue, Selecting}
import gleam/erlang/process.{Pid, Subject}
import gleam/erlang/atom.{Atom}
import gleam/otp/system
import gleam/dynamic.{Dynamic}
import gleam/int
import gleam/result
import gleam/function
import gleeunit/should

pub fn get_state_test() {
  let assert Ok(subject) =
    actor.start("Test state", fn(_msg, state) { Continue(state) })

  subject
  |> process.subject_owner
  |> system.get_state
  |> should.equal(dynamic.from("Test state"))
}

@external(erlang, "sys", "get_status")
fn get_status(a: Pid) -> Dynamic

pub fn get_status_test() {
  let assert Ok(subject) = actor.start(Nil, fn(_msg, state) { Continue(state) })

  subject
  |> process.subject_owner
  |> get_status
  // TODO: assert something about the response
}

pub fn failed_init_test() {
  actor.Spec(
    init: fn() { actor.Failed("not enough wiggles") },
    loop: fn(_msg, state) { Continue(state) },
    init_timeout: 10,
  )
  |> actor.start_spec
  |> result.is_error
  |> should.be_true
}

pub fn suspend_resume_test() {
  let assert Ok(subject) = actor.start(0, fn(_msg, iter) { Continue(iter + 1) })

  // Suspend process
  subject
  |> process.subject_owner
  |> system.suspend
  |> should.equal(Nil)

  // This normal message will not be handled yet so the state remains 0
  actor.send(subject, "hi")

  // System messages are still handled
  subject
  |> process.subject_owner
  |> system.get_state
  |> should.equal(dynamic.from(0))

  // Resume process
  subject
  |> process.subject_owner
  |> system.resume
  |> should.equal(Nil)

  // The queued regular message has been handled so the state has incremented
  subject
  |> process.subject_owner
  |> system.get_state
  |> should.equal(dynamic.from(1))
}

pub fn subject_test() {
  let assert Ok(subject) =
    actor.start("state 1", fn(msg, _state) { Continue(msg) })

  subject
  |> process.subject_owner
  |> system.get_state()
  |> should.equal(dynamic.from("state 1"))

  actor.send(subject, "state 2")

  subject
  |> process.subject_owner
  |> system.get_state()
  |> should.equal(dynamic.from("state 2"))
}

pub fn unexpected_message_test() {
  // Quieten the logger
  logger_set_primary_config(
    atom.create_from_string("level"),
    atom.create_from_string("error"),
  )

  let assert Ok(subject) =
    actor.start("state 1", fn(msg, _state) { Continue(msg) })

  subject
  |> process.subject_owner
  |> system.get_state()
  |> should.equal(dynamic.from("state 1"))

  raw_send(process.subject_owner(subject), "Unexpected message 1")
  actor.send(subject, "state 2")
  raw_send(process.subject_owner(subject), "Unexpected message 2")

  subject
  |> process.subject_owner
  |> system.get_state()
  |> should.equal(dynamic.from("state 2"))
}

pub fn unexpected_message_handled_test() {
  let assert Ok(subject) =
    actor.start_spec(actor.Spec(
      init: fn() {
        let selector =
          process.new_selector()
          |> process.selecting_anything(function.identity)
        actor.Ready(dynamic.from("init"), selector)
      },
      loop: fn(msg, _state) { Continue(msg) },
      init_timeout: 10,
    ))

  raw_send(process.subject_owner(subject), "Unexpected message 1")

  subject
  |> process.subject_owner
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
}

pub fn replace_selector_test() {
  let assert Ok(subject) =
    actor.start(
      "init",
      fn(msg: ActorMessage, state) {
        case msg {
          UserMessage(string) -> Continue("user message: " <> string)
          Unknown(val) -> Continue("unknown message: " <> dynamic.classify(val))
          SetStringSelector(reply, mapper) -> {
            let #(subject, selector) = mapped_selector(mapper)
            process.send(reply, subject)

            Selecting(state, selector)
          }
          SetIntSelector(reply, mapper) -> {
            let #(subject, selector) = mapped_selector(mapper)
            process.send(reply, subject)

            Selecting(state, selector)
          }
        }
      },
    )

  // Send initial user message to original subject
  process.send(subject, UserMessage("test 1"))
  // Check state
  get_actor_state(subject)
  |> should.equal(dynamic.from("user message: test 1"))

  // Get a new subject with string selector
  let str_subj = process.call(subject, SetStringSelector(_, UserMessage), 1000)
  // Send to new string subject
  process.send(str_subj, "test 2")
  // Check state
  get_actor_state(subject)
  |> should.equal(dynamic.from("user message: test 2"))

  // Get a new subject with int selector
  let int_subj =
    process.call(
      subject,
      SetIntSelector(_, fn(n: Int) {
        { "test " <> int.to_string(n) }
        |> UserMessage
      }),
      1000,
    )
  // Send to new int subject
  process.send(int_subj, 3)
  // Check state
  get_actor_state(subject)
  |> should.equal(dynamic.from("user message: test 3"))

  // Try to send to old string subject
  process.send(str_subj, "test 4")
  // Check state
  get_actor_state(subject)
  |> should.equal(dynamic.from("unknown message: String"))
}

fn mapped_selector(mapper: fn(a) -> ActorMessage) {
  let subject = process.new_subject()

  let selector =
    process.new_selector()
    |> process.selecting(subject, mapper)
    // Always create a selector that catches unknown messages
    |> process.selecting_anything(fn(data) {
      data
      |> dynamic.element(1, dynamic.dynamic)
      |> result.unwrap(dynamic.from("unknown"))
      |> Unknown
    })

  #(subject, selector)
}

fn get_actor_state(subject: Subject(a)) {
  subject
  |> process.subject_owner
  |> system.get_state
}

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: anything) -> anything

@external(erlang, "logger", "set_primary_config")
fn logger_set_primary_config(a: Atom, b: Atom) -> Nil
