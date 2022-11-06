import gleeunit/should
import gleam/otp/task.{Timeout}
import gleam/otp/actor.{Continue}
import gleam/erlang/process
import gleam/otp/system

type Item {
  MessageQueueLen
}

external fn process_info(pid: process.Pid, item: Item) -> #(Item, Int) =
  "erlang" "process_info"

external fn sleep(Int) -> Nil =
  "timer" "sleep"

fn work(x) {
  fn() {
    sleep(15)
    x
  }
}

pub fn async_await_test() {
  // Spawn 3 tasks, performing 45ms work collectively
  let t1 = task.async(work(1))
  let t2 = task.async(work(2))
  let t3 = task.async(work(3))

  // Assert they run concurrently (not taking 45ms total)
  task.try_await(t1, 35)
  |> should.equal(Ok(1))
  task.try_await(t2, 5)
  |> should.equal(Ok(2))
  task.try_await(t3, 5)
  |> should.equal(Ok(3))

  // Assert awaiting on previously retrieved tasks returns an error
  // An already finished task will always time out! 
  assert Error(Timeout) = task.try_await(t1, 35)
  assert Error(Timeout) = task.try_await(t2, 35)
  assert Error(Timeout) = task.try_await(t3, 35)
}

pub fn async_await_unmonitor_test() {
  // Create an actor that performs an asynchronous task
  // and monitors it until it's done
  assert Ok(subject) =
    actor.start(
      0,
      fn(_msg, state) {
        task.async(fn() { state })
        |> task.try_await(100)

        Continue(state)
      },
    )

  // We start the task in the actor
  actor.send(subject, Nil)

  // We suspend the actor, so any message sent to it
  // will remain in its mailbox
  let pid = process.subject_owner(subject)
  system.suspend(pid)

  // Actor's mailbox should not contain a "DOWN" message
  // as it should not be monitoring the completed task
  process_info(pid, MessageQueueLen)
  |> should.equal(#(MessageQueueLen, 0))
}

pub fn async_await_forever_test() {
  // Spawn 3 tasks, performing 45ms work collectively
  let t1 = task.async(work(1))
  let t2 = task.async(work(2))
  let t3 = task.async(work(3))

  // Assert they run concurrently (not caring how long they take)
  task.try_await_forever(t1)
  |> should.equal(Ok(1))
  task.try_await_forever(t2)
  |> should.equal(Ok(2))
  task.try_await_forever(t3)
  |> should.equal(Ok(3))

  //  Spawn 3 more tasks, performing 45ms work collectively
  let t4 = task.async(work(4))
  let t5 = task.async(work(5))
  let t6 = task.async(work(6))

  // Assert they run concurrently (not caring how long they take)
  task.await_forever(t4)
  |> should.equal(4)
  task.await_forever(t5)
  |> should.equal(5)
  task.await_forever(t6)
  |> should.equal(6)
}

pub fn async_await_forever_unmonitor_test() {
  // Create an actor that performs an asynchronous task
  // and monitors it until it's done
  assert Ok(subject) =
    actor.start(
      0,
      fn(_msg, state) {
        task.async(fn() { state })
        |> task.try_await_forever

        Continue(state)
      },
    )

  // We start the task in the actor
  actor.send(subject, Nil)

  // We suspend the actor, so any message sent to it
  // will remain in its mailbox
  let pid = process.subject_owner(subject)
  system.suspend(pid)

  // Actor's mailbox should not contain a "DOWN" message
  // as it should not be monitoring the completed task
  process_info(pid, MessageQueueLen)
  |> should.equal(#(MessageQueueLen, 0))
}
