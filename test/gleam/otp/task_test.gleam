import gleeunit/should
import gleam/otp/task
import gleam/otp/process

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
  assert Error(task.Exit(_)) = task.try_await(t1, 35)
  assert Error(task.Exit(_)) = task.try_await(t2, 35)
  assert Error(task.Exit(_)) = task.try_await(t3, 35)
}

pub fn asyn_await_forever_test() {
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

  // Assert awaiting on previously retrieved tasks returns an error 
  assert Error(task.Exit(_)) = task.try_await_forever(t1)
  assert Error(task.Exit(_)) = task.try_await_forever(t2)
  assert Error(task.Exit(_)) = task.try_await_forever(t3)

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
