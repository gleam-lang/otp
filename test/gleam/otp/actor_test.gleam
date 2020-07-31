import gleam/otp/actor.{Continue, Spec}
import gleam/otp/process.{Pid}
import gleam/otp/system
import gleam/dynamic.{Dynamic}
import gleam/should

pub fn get_state_test() {
  let spec = Spec(
    init: fn() { Ok("Test state") },
    loop: fn(_msg, state) { Continue(state) },
  )

  assert Ok(pid) = actor.start(spec)

  system.get_state(pid)
  |> should.equal(dynamic.from("Test state"))
}

external fn get_status(Pid) -> Dynamic =
  "sys" "get_status"

pub fn get_status_test() {
  let spec = Spec(
    init: fn() { Ok("Test state") },
    loop: fn(_msg, state) { Continue(state) },
  )
  assert Ok(pid) = actor.start(spec)
  get_status(pid)
}

pub fn suspend_resume_test() {
  let spec = Spec(
    init: fn() { Ok("Test state") },
    loop: fn(_msg, state) { Continue(state) },
  )
  assert Ok(pid) = actor.start(spec)

  // Suspend process
  system.suspend(pid)
  |> should.equal(Nil)

  // System messages are still handled
  system.get_state(pid)
  |> should.equal(dynamic.from("Test state"))

  // TODO: test normal messages are not handled.
  // Resume process
  system.resume(pid)
  |> should.equal(Nil)
}
