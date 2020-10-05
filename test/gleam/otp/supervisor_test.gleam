import gleam/should
import gleam/option.{None}
import gleam/otp/supervisor.{add, returning, worker}
import gleam/otp/process
import gleam/otp/actor

pub fn supervisor_test() {
  let tuple(sender, receiver) = process.new_channel()

  // Children send their name back to the test process during
  // initialisation so that we can tell they (re)started
  let child = fn(name) {
    fn(_) {
      actor.start(actor.Spec(
        init: fn() {
          process.send(sender, tuple(name, process.self()))
          actor.Ready(name, None)
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
  assert Ok(tuple("1", p)) = process.receive(receiver, 10)
  assert Ok(tuple("2", _)) = process.receive(receiver, 10)
  assert Ok(tuple("3", _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)

  // Kill first child an assert they all restart
  process.send_exit(p, 1)
  assert Ok(tuple("1", p1)) = process.receive(receiver, 10)
  assert Ok(tuple("2", p2)) = process.receive(receiver, 10)
  assert Ok(tuple("3", _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)

  // Kill second child an assert they all restart
  process.send_exit(p2, 1)
  assert Ok(tuple("2", _)) = process.receive(receiver, 10)
  assert Ok(tuple("3", _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)
  process.is_alive(p1)
  |> should.be_true
}
