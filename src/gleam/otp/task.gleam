// TODO: await_many
import gleam/otp/process.{Channel, Pid, ProcessMonitor}
import gleam/dynamic.{Dynamic}

pub opaque type Task(value) {
  Task(
    owner: Pid,
    pid: Pid,
    await_channel: Channel(value),
    monitor: ProcessMonitor,
  )
}

// TODO: test
// TODO: document
pub fn async(work: fn() -> value) -> Task(value) {
  let owner = process.self()
  let await_channel = process.new_channel()
  let pid = process.start(fn() { process.send(await_channel, work()) })
  let mon = process.monitor_process(pid)
  Task(owner: owner, pid: pid, await_channel: await_channel, monitor: mon)
}

pub type AwaitError {
  Timeout
  Exit(reason: Dynamic)
}

// We can only wait on a task if we are the owner of it so crash if we are
// waiting on a task we don't own.
fn assert_owner(task: Task(a)) -> Nil {
  case task.owner == process.self() {
    True -> Nil
    False -> todo
  }
}

type AwaitMessage(value) {
  Mon(process.ProcessDown)
  Chan(value)
}

// TODO: test
// TODO: document
pub fn try_await(task: Task(value), timeout: Int) -> Result(value, AwaitError) {
  assert_owner(task)
  let result =
    process.new_receiver()
    |> process.include_process_monitor(task.monitor, Mon)
    |> process.include_channel(task.await_channel, Chan)
    |> process.set_timeout(timeout)
    |> process.run_receiver
  case result {
    // The task process has sent back a value
    Ok(Chan(x)) -> {
      process.close_channel(task.await_channel)
      process.demonitor_process(task.monitor)
      Ok(x)
    }
    // The task process crashed without sending a value
    Ok(Mon(process.ProcessDown(reason: reason, ..))) -> {
      process.close_channel(task.await_channel)
      process.demonitor_process(task.monitor)
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
