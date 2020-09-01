import gleam/otp/process.{Normal}
import gleam/should
import gleam/io
import gleam/result
import gleam/atom
import gleam/dynamic
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
  let channel = process.new_channel()

  // Send message from self
  process.send(channel, 0)

  // Send message from another process
  process.start(fn() {
    process.send(channel, 1)
    process.send(channel, 2)
  })

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
  let c1 = process.new_channel()
  let c2 = process.new_channel()
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

pub fn new_reference_test() {
  let r1 = process.new_reference()
  let r2 = process.new_reference()
  r1
  |> should.not_equal(r2)
}

pub fn self_test() {
  let channel = process.new_channel()
  let child_pid1 = process.start(fn() { process.send(channel, process.self()) })
  assert Ok(child_pid2) = process.receive(channel, 100)

  child_pid1
  |> should.equal(child_pid2)

  process.self()
  |> should.not_equal(child_pid2)
}

pub fn bare_receive_test() {
  let channel = process.new_channel()
  process.send(channel, 0)
  process.untyped_send(process.self(), 1)

  let receiver =
    process.new_receiver()
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

pub fn run_receiver_forever_test() {
  let channel = process.new_channel()
  process.send(channel, 0)
  process.new_receiver()
  |> process.include_channel(channel, fn(x) { x })
  |> process.run_receiver_forever()
  |> should.equal(0)
}

pub fn pid_test() {
  let channel = process.new_channel()
  let self = process.self()
  channel
  |> process.pid
  |> should.equal(self)
}

fn call_message(value) {
  fn(reply_channel) { tuple(value, reply_channel) }
}

pub fn try_call_test() {
  let to_parent_channel = process.new_channel()

  process.start(fn() {
    // Send the call channel to the parent
    let call_channel = process.new_channel()
    process.send(to_parent_channel, call_channel)
    // Wait for the channel to be called
    assert Ok(tup) = process.receive(call_channel, 50)
    let tuple(x, reply_channel) = tup
    // Reply
    process.send(reply_channel, x + 1)
  })

  assert Ok(call_channel) = process.receive(to_parent_channel, 50)

  // Call the child process over the channel
  call_channel
  |> process.try_call(call_message(1), 50)
  |> should.equal(Ok(2))
}

pub fn try_call_timeout_test() {
  let to_parent_channel = process.new_channel()

  process.start(fn() {
    // Send the call channel to the parent
    let call_channel = process.new_channel()
    process.send(to_parent_channel, call_channel)
    // Wait for the channel to be called
    assert Ok(tup) = process.receive(call_channel, 50)
    let tuple(x, reply_channel) = tup

    // Reply, after a delay
    sleep(20)
    process.send(reply_channel, x + 1)
  })

  assert Ok(call_channel) = process.receive(to_parent_channel, 50)

  // Call the child process over the channel
  call_channel
  |> process.try_call(call_message(1), 10)
  |> result.is_error
  |> should.be_true
}

pub fn message_queue_size_test() {
  // Empty inbox
  process.new_receiver()
  |> process.include_all(fn(x) { x })
  |> process.flush_receiver

  let self = process.self()

  self
  |> process.message_queue_size
  |> should.equal(0)

  process.untyped_send(self, 1)
  process.untyped_send(self, 1)

  self
  |> process.message_queue_size
  |> should.equal(2)
}

pub fn monitor_test_test() {
  // Spawn child
  let to_parent_channel = process.new_channel()
  let pid =
    process.start(fn() {
      let channel = process.new_channel()
      process.send(to_parent_channel, channel)
      process.receive(channel, 150)
    })

  // Monitor child
  let monitor = process.monitor_process(pid)

  // Shutdown child to trigger monitor
  assert Ok(channel) = process.receive(to_parent_channel, 50)
  process.send(channel, Nil)

  // We get a process down message!
  process.new_receiver()
  |> process.include_process_monitor(monitor, fn(x) { x })
  |> process.set_timeout(5)
  |> process.run_receiver
  |> should.equal(Ok(process.ProcessDown(pid, dynamic.from(process.Normal))))
}

pub fn demonitor_test_test() {
  // Spawn child
  let to_parent_channel = process.new_channel()
  let pid =
    process.start(fn() {
      let channel = process.new_channel()
      process.send(to_parent_channel, channel)
      process.receive(channel, 150)
    })

  // Monitor child
  let monitor = process.monitor_process(pid)

  // Shutdown child to trigger monitor
  assert Ok(channel) = process.receive(to_parent_channel, 50)
  process.send(channel, Nil)

  // Demonitor, which will flush the messages
  process.demonitor_process(monitor)

  // We don't get a process down message as we demonitored the child
  process.new_receiver()
  |> process.include_process_monitor(monitor, fn(x) { x })
  |> process.set_timeout(5)
  |> process.run_receiver
  |> should.equal(Error(Nil))
}

pub fn set_timeout_test() {
  let channel = process.new_channel()
  process.start(fn() {
    sleep(10)
    process.send(channel, Nil)
  })

  let receiver =
    process.new_receiver()
    |> process.include_channel(channel, fn(x) { x })
    |> process.set_timeout(0)

  receiver
  |> process.run_receiver
  |> should.equal(Error(Nil))

  receiver
  |> process.set_timeout(20)
  |> process.run_receiver
  |> should.equal(Ok(Nil))
}

pub fn flush_other_test() {
  let c1 = process.new_channel()
  let c2 = process.new_channel()

  process.send(c1, 0)
  process.send(c2, 0)

  let receiver =
    process.new_receiver()
    |> process.flush_other(True)
    |> process.include_channel(c2, fn(x) { x })
    |> process.set_timeout(0)

  receiver
  |> process.run_receiver()
  |> should.equal(Ok(0))

  // The other message was also dropped
  receiver
  |> process.include_channel(c1, fn(x) { x })
  |> process.run_receiver()
  |> should.equal(Error(Nil))
}

pub fn wrap_channel_test() {
  let channel = process.new_channel()
  let wrapped = process.wrap_channel(channel, fn(x) { tuple(x, x) })
  process.send(wrapped, "Wot")
  channel
  |> process.receive(0)
  |> should.equal(Ok(tuple("Wot", "Wot")))
}

pub fn null_channel_test() {
  let channel = process.null_channel(process.self())
  process.send(channel, 0)
  channel
  |> process.receive(0)
  |> should.equal(Error(Nil))
}
