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

pub fn bare_receive_test() {
  let channel = process.make_channel()
  process.send(channel, 0)
  process.unsafe_send(process.self(), 1)

  let receiver = process.make_receiver()
    |> process.include_bare(fn(x) { x })
    |> process.set_timeout(0)

  // The channel message is skipped over for the bare message
  receiver
  |> process.run_receiver()
  |> should.equal(Ok(dynamic.from(1)))
  receiver
  |> process.run_receiver()
  |> should.equal(Error(Nil))
}

pub fn pid_test() {
  let channel = process.make_channel()
  let self = process.self()
  channel
  |> process.pid
  |> should.equal(self)
}

pub fn try_call_test() {
  let to_parent_channel = process.make_channel()

  process.start(
    fn() {
      // Send the call channel to the parent
      let call_channel = process.make_channel()
      process.send(to_parent_channel, call_channel)
      // Wait for the channel to be called
      assert Ok(tup) = process.receive(call_channel, 50)
      let tuple(x, reply_channel) = tup
      // Reply
      process.send(reply_channel, x + 1)
    },
  )

  assert Ok(call_channel) = process.receive(to_parent_channel, 50)

  // Call the child process over the channel
  call_channel
  |> process.try_call(1, 50)
  |> should.equal(Ok(2))
}

pub fn try_call_timeout_test() {
  let to_parent_channel = process.make_channel()

  process.start(
    fn() {
      // Send the call channel to the parent
      let call_channel = process.make_channel()
      process.send(to_parent_channel, call_channel)
      // Wait for the channel to be called
      assert Ok(tup) = process.receive(call_channel, 50)
      let tuple(x, reply_channel) = tup

      // Reply, after a delay
      sleep(20)
      process.send(reply_channel, x + 1)
    },
  )

  assert Ok(call_channel) = process.receive(to_parent_channel, 50)

  // Call the child process over the channel
  call_channel
  |> process.try_call(1, 10)
  |> result.is_error
  |> should.be_true
}

pub fn message_queue_size() {
  let self = process.self()

  self
  |> process.message_queue_size
  |> should.equal(0)

  process.unsafe_send(self, 1)
  process.unsafe_send(self, 1)

  self
  |> process.message_queue_size
  |> should.equal(2)
}
