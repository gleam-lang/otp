import gleam/otp/actor.{Continue, Spec}
import gleam/otp/process.{Pid}
import gleam/otp/system
import gleam/dynamic.{Dynamic}
import gleam/should
import gleam/result

pub fn get_state_test() {
  let spec =
    Spec(
      init: fn() { Ok("Test state") },
      loop: fn(_msg, state) { Continue(state) },
    )

  assert Ok(channel) = actor.start(spec)

  channel
  |> process.pid
  |> system.get_state
  |> should.equal(dynamic.from("Test state"))
}

external fn get_status(Pid) -> Dynamic =
  "sys" "get_status"

pub fn get_status_test() {
  let spec =
    Spec(
      init: fn() { Ok("Test state") },
      loop: fn(_msg, state) { Continue(state) },
    )
  assert Ok(channel) = actor.start(spec)
  channel
  |> process.pid
  |> get_status
  // TODO: assert something about the response
}

pub fn failed_init_test() {
  Spec(
    init: fn() { Error(process.Normal) },
    loop: fn(_msg, state) { Continue(state) },
  )
  |> actor.start
  |> result.is_error
  |> should.be_true
}

pub fn suspend_resume_test() {
  let spec =
    Spec(
      init: fn() { Ok("Test state") },
      loop: fn(_msg, state) { Continue(state) },
    )
  assert Ok(channel) = actor.start(spec)

  // Suspend process
  channel
  |> process.pid
  |> system.suspend
  |> should.equal(Nil)

  // System messages are still handled
  channel
  |> process.pid
  |> system.get_state
  |> should.equal(dynamic.from("Test state"))

  // TODO: test normal messages are not handled.
  // Resume process
  channel
  |> process.pid
  |> system.resume
  |> should.equal(Nil)
}

pub fn channel_test() {
  let spec =
    Spec(
      init: fn() { Ok("Test state") },
      loop: fn(msg, _state) { Continue(msg) },
    )

  assert Ok(channel) = actor.start(spec)
  channel
  |> process.pid
  |> system.get_state()
  |> should.equal(dynamic.from("Test state"))

  actor.send(channel, "testing")

  channel
  |> process.pid
  |> system.get_state()
  |> should.equal(dynamic.from("testing"))
}
