import gleam/otp/actor.{Continue}
import gleam/otp/process.{Pid}
import gleam/otp/system
import gleam/dynamic.{Dynamic}
import gleeunit/should
import gleam/result

pub fn get_state_test() {
  assert Ok(channel) =
    actor.start("Test state", fn(_msg, state) { Continue(state) })

  channel
  |> process.pid
  |> system.get_state
  |> should.equal(dynamic.from("Test state"))
}

external fn get_status(Pid) -> Dynamic =
  "sys" "get_status"

pub fn get_status_test() {
  assert Ok(channel) = actor.start(Nil, fn(_msg, state) { Continue(state) })

  channel
  |> process.pid
  |> get_status
  // TODO: assert something about the response
}

pub fn failed_init_test() {
  actor.Spec(
    init: fn() { actor.Failed(process.Normal) },
    loop: fn(_msg, state) { Continue(state) },
    init_timeout: 10,
  )
  |> actor.start_spec
  |> result.is_error
  |> should.be_true
}

pub fn suspend_resume_test() {
  assert Ok(channel) = actor.start(0, fn(_msg, iter) { Continue(iter + 1) })

  // Suspend process
  channel
  |> process.pid
  |> system.suspend
  |> should.equal(Nil)

  // This normal message will not be handled yet so the state remains 0
  actor.send(channel, "hi")

  // System messages are still handled
  channel
  |> process.pid
  |> system.get_state
  |> should.equal(dynamic.from(0))

  // Resume process
  channel
  |> process.pid
  |> system.resume
  |> should.equal(Nil)

  // The queued regular message has been handled so the state has incremented
  channel
  |> process.pid
  |> system.get_state
  |> should.equal(dynamic.from(1))
}

pub fn channel_test() {
  assert Ok(channel) = actor.start("state 1", fn(msg, _state) { Continue(msg) })

  channel
  |> process.pid
  |> system.get_state()
  |> should.equal(dynamic.from("state 1"))

  actor.send(channel, "state 2")

  channel
  |> process.pid
  |> system.get_state()
  |> should.equal(dynamic.from("state 2"))
}
