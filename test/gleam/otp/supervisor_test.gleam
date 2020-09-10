import gleam/should
import gleam/otp/supervisor.{add, update_argument, worker}
import gleam/otp/process
import gleam/otp/actor

pub fn supervisor_test() {
  let tuple(sender, receiver) = process.new_channel()

  let child = fn(name) {
    fn(_) {
      actor.start(actor.Spec(
        init: fn() {
          process.send(sender, tuple(name, process.self()))
          Ok(name)
        },
        init_timeout: 10,
        loop: fn(_msg, state) { actor.Continue(state) },
      ))
    }
  }

  supervisor.start(fn(children) {
    children
    |> add(worker(child("1")))
    |> add(worker(child("2")))
    |> add(worker(child("3")))
  })
  |> should.be_ok

  // Assert children have started
  assert Ok(tuple("1", _p)) = process.receive(receiver, 10)
  assert Ok(tuple("2", _)) = process.receive(receiver, 10)
  assert Ok(tuple("3", _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)

  // // Kill first child an assert they all restart
  // process.send_exit(p, 1)
  // assert Ok(tuple("1", _)) = process.receive(receiver, 10)
  // assert Ok(tuple("2", p)) = process.receive(receiver, 10)
  // assert Ok(tuple("3", _)) = process.receive(receiver, 10)
  // assert Error(Nil) = process.receive(receiver, 10)
  Nil
}
