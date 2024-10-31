import gleam/erlang/process.{type Pid}
import gleam/otp/task.{Timeout}
import gleeunit/should

@external(erlang, "gleam_otp_test_external", "flush")
fn flush() -> Nil

@external(erlang, "gleam_otp_test_external", "get_message_queue_length")
fn get_message_queue_length(pid pid: Pid) -> Int

@external(erlang, "timer", "sleep")
fn sleep(a: Int) -> Nil

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
  let assert Error(Timeout) = task.try_await(t1, 35)
  let assert Error(Timeout) = task.try_await(t2, 35)
  let assert Error(Timeout) = task.try_await(t3, 35)
}

pub fn async_await_unmonitor_test() {
  // Start with an empty mailbox
  flush()

  // Perform an asynchronous task
  // and monitor it until it's done
  let _result =
    task.async(work(1))
    |> task.try_await(50)

  // Mailbox should be empty;
  // no "DOWN" message should have been sent
  process.self()
  |> get_message_queue_length
  |> should.equal(0)
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
  // Start with an empty mailbox
  flush()

  // Perform an asynchronous task
  // and monitor it until it's done
  let _result =
    task.async(work(1))
    |> task.try_await_forever

  // Mailbox should be empty;
  // no "DOWN" message should have been sent
  process.self()
  |> get_message_queue_length
  |> should.equal(0)
}

pub fn try_await2_test() {
  // Start with an empty mailbox
  flush()

  let work = fn(x) {
    fn() {
      sleep(5)
      x
    }
  }

  let task1 = task.async(work(1))
  let task2 = task.async(work(2))

  task.try_await2(task1, task2, 8)
  |> should.equal(#(Ok(1), Ok(2)))
}

pub fn try_await2_timeout_test() {
  // Start with an empty mailbox
  flush()

  let work = fn(x, y) {
    fn() {
      sleep(y)
      x
    }
  }

  // 2 will not finish in time
  let task1 = task.async(work(1, 0))
  let task2 = task.async(work(2, 10))

  task.try_await2(task1, task2, 5)
  |> should.equal(#(Ok(1), Error(Timeout)))
}

pub fn try_await3_test() {
  // Start with an empty mailbox
  flush()

  let work = fn(x) {
    fn() {
      sleep(5)
      x
    }
  }

  let task1 = task.async(work(1))
  let task2 = task.async(work(2))
  let task3 = task.async(work(3))

  task.try_await3(task1, task2, task3, 8)
  |> should.equal(#(Ok(1), Ok(2), Ok(3)))
}

pub fn try_await3_timeout_test() {
  // Start with an empty mailbox
  flush()

  let work = fn(x, y) {
    fn() {
      sleep(y)
      x
    }
  }

  // 1 will not finish in time
  let task1 = task.async(work(1, 100))
  let task2 = task.async(work(2, 1))
  let task3 = task.async(work(3, 1))

  task.try_await3(task1, task2, task3, 20)
  |> should.equal(#(Error(Timeout), Ok(2), Ok(3)))
}

pub fn try_await4_test() {
  // Start with an empty mailbox
  flush()

  let work = fn(x) {
    fn() {
      sleep(5)
      x
    }
  }

  let task1 = task.async(work(1))
  let task2 = task.async(work(2))
  let task3 = task.async(work(3))
  let task4 = task.async(work(4))

  task.try_await4(task1, task2, task3, task4, 8)
  |> should.equal(#(Ok(1), Ok(2), Ok(3), Ok(4)))
}

pub fn try_await4_timeout_test() {
  // Start with an empty mailbox
  flush()

  let work = fn(x, y) {
    fn() {
      sleep(y)
      x
    }
  }

  // 1 will not finish in time
  let task1 = task.async(work(1, 100))
  let task2 = task.async(work(2, 1))
  let task3 = task.async(work(3, 1))
  let task4 = task.async(work(4, 1))

  task.try_await4(task1, task2, task3, task4, 20)
  |> should.equal(#(Error(Timeout), Ok(2), Ok(3), Ok(4)))
}
