import gleam/otp/process.{ExitReason, Message, Normal}
import gleam/should
import gleam/io
import gleam/result
import gleam/atom
import gleam/dynamic.{Dynamic}
import gleam/option.{Some}

external fn sleep(Int) -> Nil =
  "timer" "sleep"

pub fn is_alive_test() {
  let pid = process.start(fn() { sleep(1000) })
  pid
  |> process.is_alive
  |> should.equal(True)
}

pub fn is_alive_dead_test() {
  let pid = process.start(fn() { Nil })
  sleep(20)
  pid
  |> process.is_alive
  |> should.equal(False)
}

pub fn receive_test() {
  let channel = process.make_channel()

  // Send message from self
  process.send(channel, 0)

  // Send message from another process
  process.start(
    fn() {
      process.send(channel, 1)
      process.send(channel, 2)
    },
  )

  // Assert all the messages arrived
  process.receive(channel, 0)
  |> should.equal(Ok(0))
  process.receive(channel, 50)
  |> should.equal(Ok(1))
  process.receive(channel, 0)
  |> should.equal(Ok(2))
  process.receive(channel, 0)
  |> should.equal(Error(Nil))
}

pub fn flush_test() {
  let c1 = process.make_channel()
  let c2 = process.make_channel()
  process.send(c1, 1)
  process.send(c2, 2)
  process.send(c2, 3)

  // Flush c2
  process.flush(c2)
  |> should.equal(2)

  // c2 messages have been dropped
  process.receive(c2, 0)
  |> should.equal(Error(Nil))

  // c1 still has messages
  process.receive(c1, 0)
  |> should.equal(Ok(1))
}

pub fn make_reference_test() {
  let r1 = process.make_reference()
  let r2 = process.make_reference()
  r1
  |> should.not_equal(r2)
}

pub fn self_test() {
  let channel = process.make_channel()
  let child_pid1 = process.start(fn() { process.send(channel, process.self()) })
  assert Ok(child_pid2) = process.receive(channel, 100)

  child_pid1
  |> should.equal(child_pid2)

  process.self()
  |> should.not_equal(child_pid2)
}
