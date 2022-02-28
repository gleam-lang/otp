import gleam/otp/process
import gleam/otp/port.{Port}
import gleeunit/should
import gleam/io
import gleam/result
import gleam/erlang/atom
import gleam/dynamic
import gleam/option

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
  let #(sender, receiver) = process.new_channel()

  // Send message from self
  process.send(sender, 0)

  // Send message from another process
  process.start(fn() {
    process.send(sender, 1)
    process.send(sender, 2)
  })

  // Assert all the messages arrived
  process.receive(receiver, 0)
  |> should.equal(Ok(0))
  process.receive(receiver, 50)
  |> should.equal(Ok(1))
  process.receive(receiver, 0)
  |> should.equal(Ok(2))
  process.receive(receiver, 0)
  |> should.equal(Error(Nil))
}

pub fn flush_test() {
  let #(s1, r1) = process.new_channel()
  let #(s2, r2) = process.new_channel()
  process.send(s1, 1)
  process.send(s2, 2)
  process.send(s2, 3)

  // Flush channel 1
  process.flush(r2)
  |> should.equal(2)

  // channel 2 messages have been dropped
  process.receive(r2, 0)
  |> should.equal(Error(Nil))

  // channel 1 still has messages
  process.receive(r1, 0)
  |> should.equal(Ok(1))
}

pub fn new_reference_test() {
  let r1 = process.new_reference()
  let r2 = process.new_reference()
  r1
  |> should.not_equal(r2)
}

pub fn self_test() {
  let #(sender, receiver) = process.new_channel()
  let child_pid1 = process.start(fn() { process.send(sender, process.self()) })
  assert Ok(child_pid2) = process.receive(receiver, 100)

  child_pid1
  |> should.equal(child_pid2)

  process.self()
  |> should.not_equal(child_pid2)
}

pub fn bare_receive_test() {
  let #(sender, receiver) = process.new_channel()
  process.send(sender, 0)
  process.untyped_send(process.self(), 1)

  // The channel message is skipped over for the bare message
  process.bare_message_receiver()
  |> process.receive(0)
  |> should.equal(Ok(dynamic.from(1)))

  process.bare_message_receiver()
  |> process.receive(0)
  |> should.equal(Error(Nil))

  // The channel message is still in the queue
  receiver
  |> process.receive(0)
  |> should.equal(Ok(0))
}

pub fn bare_receive_port_test() {
  // Generate a port message
  let _port = open_port(Spawn("gleam --version"), [ExitStatus])

  // The channel recieves the stdout from the port
  process.bare_message_receiver()
  |> process.receive(100)
  |> should.be_ok()
}

pub fn run_receiver_forever_test() {
  let #(sender, receiver) = process.new_channel()
  process.send(sender, 0)
  receiver
  |> process.receive_forever()
  |> should.equal(0)
}

pub fn trap_exits_test() {
  let receiver = process.trap_exits()
  // TODO: test that exit signals are trapped
  process.close_channels(receiver)
  // TODO: test that exit signals are not trapped
}

pub fn pid_test() {
  let #(sender, _) = process.new_channel()
  let self = process.self()
  sender
  |> process.pid
  |> should.equal(self)
}

fn call_message(value) {
  fn(reply_channel) { #(value, reply_channel) }
}

pub fn try_call_test() {
  let #(parent_sender, parent_receiver) = process.new_channel()

  process.start(fn() {
    // Send the call channel to the parent
    let #(call_sender, call_receiver) = process.new_channel()
    process.send(parent_sender, call_sender)
    // Wait for the channel to be called
    assert Ok(#(x, reply_channel)) = process.receive(call_receiver, 50)
    // Reply
    process.send(reply_channel, x + 1)
  })

  assert Ok(call_sender) = process.receive(parent_receiver, 50)

  // Call the child process over the channel
  call_sender
  |> process.try_call(call_message(1), 50)
  |> should.equal(Ok(2))
}

pub fn try_call_timeout_test() {
  let #(parent_sender, parent_receiver) = process.new_channel()

  process.start(fn() {
    // Send the call channel to the parent
    let #(call_sender, call_receiver) = process.new_channel()
    process.send(parent_sender, call_sender)
    // Wait for the channel to be called
    assert Ok(#(x, reply_channel)) = process.receive(call_receiver, 50)
    // Reply, after a delay
    sleep(20)
    process.send(reply_channel, x + 1)
  })

  assert Ok(call_sender) = process.receive(parent_receiver, 50)

  // Call the child process over the channel
  call_sender
  |> process.try_call(call_message(1), 10)
  |> result.is_error
  |> should.be_true
}

// TODO
// pub fn message_queue_size_test() {
//   // Empty inbox
//   process.new_receiver()
//   |> process.include_all(fn(x) { x })
//   |> process.flush_receiver
//   let self = process.self()
//   self
//   |> process.message_queue_size
//   |> should.equal(0)
//   process.untyped_send(self, 1)
//   process.untyped_send(self, 1)
//   self
//   |> process.message_queue_size
//   |> should.equal(2)
// }
pub fn monitor_test_test() {
  // Spawn child
  let #(send_to_parent, parent_receiver) = process.new_channel()
  let pid =
    process.start(fn() {
      let #(sender, receiver) = process.new_channel()
      process.send(send_to_parent, sender)
      process.receive(receiver, 150)
    })

  // Monitor child
  let monitor = process.monitor_process(pid)

  // Shutdown child to trigger monitor
  assert Ok(sender) = process.receive(parent_receiver, 50)
  process.send(sender, Nil)

  // We get a process down message!
  monitor
  |> process.receive(5)
  |> should.equal(Ok(process.ProcessDown(pid, dynamic.from(process.Normal))))
}

// pub fn flush_other_test() {
//   let c1 = process.new_channel()
//   let c2 = process.new_channel()
//
//   process.send(c1, 0)
//   process.send(c2, 0)
//
//   let receiver =
//     process.new_receiver()
//     |> process.flush_other(True)
//     |> process.include_channel(c2, fn(x) { x })
//     |> process.set_timeout(0)
//
//   receiver
//   |> process.run_receiver()
//   |> should.equal(Ok(0))
//
//   // The other message was also dropped
//   receiver
//   |> process.include_channel(c1, fn(x) { x })
//   |> process.run_receiver()
//   |> should.equal(Error(Nil))
// }
//
pub fn null_channel_test() {
  let queue_size = process.message_queue_size(process.self())
  process.self()
  |> process.null_sender
  |> process.send(100)
  process.self()
  |> process.message_queue_size()
  |> should.equal(queue_size)
}

pub fn send_after_test() {
  let #(sender, receiver) = process.new_channel()

  // 0 is received immediately, though asynchronously
  process.send_after(sender, 0, "a")
  receiver
  |> process.receive(5)
  |> should.equal(Ok("a"))

  // With a delay it is sent later
  process.send_after(sender, 5, "b")
  receiver
  |> process.receive(0)
  |> should.equal(Error(Nil))
  receiver
  |> process.receive(20)
  |> should.equal(Ok("b"))
}

pub fn null_channel_send_after_test() {
  process.self()
  |> process.null_sender
  |> process.send_after(0, "a")
}

pub fn cancel_timer_test() {
  let #(sender, _receiver) = process.new_channel()
  let instant_timer = process.send_after(sender, 0, "a")
  let later_timer = process.send_after(sender, 100, "a")
  sleep(5)
  assert process.TimerNotFound = process.cancel_timer(instant_timer)
  assert process.Cancelled(i) = process.cancel_timer(later_timer)
  should.be_true(i > 0)
  should.be_true(i < 100)
}

pub fn map_receiver_test() {
  let #(sender, receiver) = process.new_channel()
  let receiver =
    receiver
    |> process.map_receiver(fn(x) { x + 1 })
    |> process.map_receiver(fn(x) { [x] })

  process.send(sender, 1)
  process.send(sender, 2)

  receiver
  |> process.receive(0)
  |> should.equal(Ok([2]))

  receiver
  |> process.receive(0)
  |> should.equal(Ok([3]))
}

pub fn map_sender_test() {
  let #(sender, receiver) = process.new_channel()
  let sender =
    sender
    |> process.map_sender(fn(x) { [x] })
    |> process.map_sender(fn(x) { x + 1 })

  process.send(sender, 1)
  process.send(sender, 2)

  receiver
  |> process.receive(0)
  |> should.equal(Ok([2]))

  receiver
  |> process.receive(0)
  |> should.equal(Ok([3]))
}

type PortName {
  Spawn(command: String)
}

type PortSettings {
  ExitStatus
}

external fn open_port(PortName, List(PortSettings)) -> Port =
  "erlang" "open_port"

pub fn dynamic_is_pid_test() {
  let pid = process.start(fn() { Nil })
  let dynamic = dynamic.from(pid)
  process.pid_from_dynamic(dynamic)
  |> should.equal(Ok(pid))
}

pub fn dynamic_is_not_pid_test() {
  let not_pid = "ceci n'est pas un pid"
  let dynamic = dynamic.from(not_pid)
  process.pid_from_dynamic(dynamic)
  |> should.equal(Error([
    dynamic.DecodeError(expected: "Pid", found: "String", path: []),
  ]))
}
