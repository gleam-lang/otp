import gleam/otp/actor.{Spec, Continue}
import gleam/otp/process.{Pid}
import gleam/otp/system
import gleam/dynamic.{Dynamic}
import gleam/should

pub fn get_state_test() {
  let spec = Spec(
    init: fn(_pid) { Ok("Test state") },
    loop: fn(_msg, state) { Continue(state) },
  )

  assert Ok(pid) = actor.start(spec)

  system.get_state(pid)
  |> should.equal(dynamic.from("Test state"))
}
