import gleeunit/should
import gleam/option.{None}
import gleam/otp/supervisor.{add, returning, worker}
import gleam/otp/process
import gleam/otp/actor

pub fn supervisor_test() {
  let #(sender, receiver) = process.new_channel()

  // Children send their name back to the test process during
  // initialisation so that we can tell they (re)started
  let child =
    worker(fn(name) {
      actor.start_spec(actor.Spec(
        init: fn() {
          process.send(sender, #(name, process.self()))
          actor.Ready(name, None)
        },
        init_timeout: 10,
        loop: fn(_msg, state) { actor.Continue(state) },
      ))
    })

  // Each child returns the next name, which is their name + 1
  let child =
    child
    |> returning(fn(name, _sender) { name + 1 })

  supervisor.start_spec(supervisor.Spec(
    argument: 1,
    frequency_period: 1,
    max_frequency: 5,
    init: fn(children) {
      children
      |> add(child)
      |> add(child)
      |> add(child)
    },
  ))
  |> should.be_ok

  // Assert children have started
  assert Ok(#(1, p)) = process.receive(receiver, 10)
  assert Ok(#(2, _)) = process.receive(receiver, 10)
  assert Ok(#(3, _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)

  // Kill first child an assert they all restart
  process.send_exit(p, 1)
  assert Ok(#(1, p1)) = process.receive(receiver, 10)
  assert Ok(#(2, p2)) = process.receive(receiver, 10)
  assert Ok(#(3, _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)

  // Kill second child an assert they all restart
  process.send_exit(p2, 1)
  assert Ok(#(2, _)) = process.receive(receiver, 10)
  assert Ok(#(3, _)) = process.receive(receiver, 10)
  assert Error(Nil) = process.receive(receiver, 10)
  process.is_alive(p1)
  |> should.be_true
}
