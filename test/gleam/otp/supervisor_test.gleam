import gleam/should
import gleam/otp/supervisor.{add, update_argument, worker}
import gleam/otp/process.{Sender}
import gleam/otp/actor.{StartError}

// Testing
pub fn start_child1(x: Nil) -> Result(Sender(msg), StartError) {
  actor.new(x, fn(_msg, state) { actor.Continue(state) })
}

pub fn start_child2(x: Sender(msg)) -> Result(Sender(msg), StartError) {
  actor.new(x, fn(_msg, state) { actor.Continue(state) })
}

pub fn start_child3(x: Sender(msg)) -> Result(Sender(msg), StartError) {
  actor.new(x, fn(_msg, state) { actor.Continue(state) })
}

pub fn supervisor_test() {
  fn(children) {
    children
    |> add(
      worker(start_child1)
      |> update_argument(fn(_arg, channel) { channel }),
    )
    |> add(worker(start_child2))
    |> add(worker(start_child3))
  }
  |> supervisor.start
  |> should.be_ok
}
