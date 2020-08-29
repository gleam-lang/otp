import gleam/otp/supervisor.{add, new_children, update_argument, worker_child}
import gleam/otp/process.{Channel}
import gleam/otp/actor.{StartError}

// Testing
pub fn start_child1(x: Nil) -> Result(Channel(msg), StartError) {
  actor.new(x, fn(_msg, state) { actor.Continue(state) })
}

pub fn start_child2(x: Channel(msg)) -> Result(Channel(msg), StartError) {
  actor.new(x, fn(_msg, state) { actor.Continue(state) })
}

pub fn start_child3(x: Channel(msg)) -> Result(Channel(msg), StartError) {
  actor.new(x, fn(_msg, state) { actor.Continue(state) })
}

pub fn supervisor_test() {
  Nil
  |> new_children
  |> add(
    worker_child(start_child1)
    |> update_argument(fn(_arg, pid) { pid }),
  )
  |> add(worker_child(start_child2))
  |> add(worker_child(start_child3))
}
