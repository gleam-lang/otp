//// A task is a kind of process that performs a single task and then shuts
//// down. Commonly tasks are used to convert sequential code into concurrent
//// code by performing computation in another process.
////
////    let t = task.async(fn() { do_some_work() })
////    res = do_some_other_work()
////    res + task.await(t, 100)
////
//// Tasks spawned with async can be awaited on by their caller process (and
//// only their caller) as shown in the example above. They are implemented by
//// spawning a process that sends a message to the caller once the given
//// computation is performed.
////
//// There are two important things to consider when using `async`:
////
//// 1. If you are using async tasks, you must await a reply as they are always
////    sent.
////
//// 2. async tasks link the caller and the spawned process. This means that,
////    if the caller crashes, the task will crash too and vice-versa. This is
////    on purpose: if the process meant to receive the result no longer
////    exists, there is no purpose in completing the computation.
////
//// This module is inspired by Elixir's [Task module][1].
////
//// [1]: https://hexdocs.pm/elixir/master/Task.html
////

// TODO: await_many
import gleam/otp/process.{Pid, Receiver}
import gleam/dynamic.{Dynamic}

pub opaque type Task(value) {
  Task(owner: Pid, pid: Pid, receiver: Receiver(Message(value)))
}

// TODO: test
/// Spawn a task process that calls a given function in order to perform some
/// work. The result of this function is send back to the parent and can be
/// received using the `await` function.
///
/// See the top level module documentation for more information on async/await.
///
pub fn async(work: fn() -> value) -> Task(value) {
  let owner = process.self()
  let tuple(sender, receiver) = process.new_channel()
  let pid = process.start(fn() { process.send(sender, work()) })
  let receiver =
    pid
    |> process.monitor_process
    |> process.map_receiver(Mon)
    |> process.merge_receiver(
      receiver
      |> process.map_receiver(Chan),
    )
  Task(owner: owner, pid: pid, receiver: receiver)
}

pub type AwaitError {
  Timeout
  Exit(reason: Dynamic)
}

// We can only wait on a task if we are the owner of it so crash if we are
// waiting on a task we don't own.
fn assert_owner(task: Task(a)) -> Nil {
  let self = process.self()
  case task.owner == self {
    True -> Nil
    False ->
      process.send_exit(
        to: self,
        because: "awaited on a task that does not belong to this process",
      )
  }
}

type Message(value) {
  Mon(process.ProcessDown)
  Chan(value)
}

// TODO: test
// TODO: document
pub fn try_await(task: Task(value), timeout: Int) -> Result(value, AwaitError) {
  assert_owner(task)
  case process.receive(task.receiver, timeout) {
    // The task process has sent back a value
    Ok(Chan(x)) -> {
      process.close_channels(task.receiver)
      Ok(x)
    }

    // The task process crashed without sending a value
    Ok(Mon(process.ProcessDown(reason: reason, ..))) -> {
      process.close_channels(task.receiver)
      Error(Exit(reason))
    }

    // The task process is alive but has not sent a value yet
    Error(Nil) -> Error(Timeout)
  }
}

// TODO: test
/// Wait for the value computed by a task.
///
/// If the a value is not received before the timeout has elapsed or if the
/// task process crashes then this function crashes.
///
pub fn await(task: Task(value), timeout: Int) -> value {
  assert Ok(value) = try_await(task, timeout)
  value
}
