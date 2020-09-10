// TODO: await_many
import gleam/otp/process.{Pid, Receiver}
import gleam/dynamic.{Dynamic}

pub opaque type Task(value) {
  Task(owner: Pid, pid: Pid, receiver: Receiver(Message(value)))
}

// TODO: test
// TODO: document
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
// TODO: document
pub fn await(task: Task(value), timeout: Int) -> value {
  assert Ok(value) = try_await(task, timeout)
  value
}
