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

pub fn channel_receive_test() {
  let channel = process.make_channel()

  // Send message from self
  process.channel_send(channel, 0)

  // Send message from another process
  process.start(
    fn() {
      process.channel_send(channel, 1)
      process.channel_send(channel, 2)
    },
  )

  // Assert all the messages arrived
  process.channel_receive(channel, 0)
  |> should.equal(Ok(0))
  process.channel_receive(channel, 50)
  |> should.equal(Ok(1))
  process.channel_receive(channel, 0)
  |> should.equal(Ok(2))
  process.channel_receive(channel, 0)
  |> should.equal(Error(Nil))
}

pub fn flush_channel_messages_test() {
  let c1 = process.make_channel()
  let c2 = process.make_channel()
  process.channel_send(c1, 1)
  process.channel_send(c2, 2)
  process.channel_send(c2, 3)

  // Flush c2
  process.flush_channel_messages([c2])
  |> should.equal(2)

  // c2 messages have been dropped
  process.channel_receive(c2, 0)
  |> should.equal(Error(Nil))

  // c1 still has messages
  process.channel_receive(c1, 0)
  |> should.equal(Ok(1))
}

pub fn channels_receive_test() {
  let c1 = process.make_channel()
  let c2 = process.make_channel()
  process.channel_send(c1, 1)
  process.channels_receive([c1, c2], 0)
  |> should.equal(Ok(tuple(c1, 1)))
}

//pub fn async_send_test() {
//  assert Ok(
//    pid,
//  ) = process.start(
//    fn() {
//      self
//      |> process.receive(50)
//      |> should.equal(Ok(Message(1)))
//      self
//      |> process.receive(50)
//      |> should.equal(Ok(Message(2)))
//      self
//      |> process.receive(50)
//      |> should.equal(Ok(Message(3)))
//      Normal
//    },
//  )
//  let resp = process.async_send(pid, 1)
//  should.equal(resp, Nil)
//  let resp = process.async_send(pid, 2)
//  should.equal(resp, Nil)
//  let resp = process.async_send(pid, 3)
//  should.equal(resp, Nil)
//}
// type EchoMessage(x) {
//   EchoMessage(From(x), x)
// }
//pub fn sync_send_test() {
//  assert Ok(
//    pid,
//  ) = process.start(
//    fn() {
//      assert Ok(Message(EchoMessage(from, x))) = process.receive(self, 50)
//      assert Nil = process.reply(from, x)
//      assert Ok(Message(EchoMessage(from, x))) = process.receive(self, 50)
//      assert Nil = process.reply(from, x)
//      assert Ok(Message(EchoMessage(from, x))) = process.receive(self, 50)
//      assert Nil = process.reply(from, x)
//      Normal
//    },
//  )
//  let resp = process.sync_send(pid, EchoMessage(_, 1), 50)
//  should.equal(resp, 1)
//  let resp = process.sync_send(pid, EchoMessage(_, 2), 50)
//  should.equal(resp, 2)
//  let resp = process.sync_send(pid, EchoMessage(_, 2.0), 50)
//  should.equal(resp, 2.0)
//}
//pub fn unsafe_downcast_send() {
//  let f = fn(handle: fn(x) -> x) {
//    fn() {
//      let Message(msg) = process.receive(self, 1000)
//      handle(msg)
//    }
//  }
//  assert Ok(float_pid) = process.start(f(fn(x) { x +. 1. }))
//  assert Ok(int_pid) = process.start(f(fn(x) { x + 1 }))
//  let opaque_pid = process.make_opaque(int_pid)
//  let fake_float_pid = process.unsafe_downcast(opaque_pid)
//  // They can be compared now, they are the same type
//  should.equal(float_pid, fake_float_pid)
//}
type HandleExit {
  HandleExit(exited: process.Pid, reason: ExitReason)
  Ping
}
// pub fn trap_exit_test() {
//   let linkee_routine = fn() { process.receive(self, 150) }
//   let linkee = process.start(linkee_routine)
//
//   let routine = fn(self) {
//     process.started(self)
//     let expected_exit_signal = HandleExit(process.make_opaque(linkee), Normal)
//     self
//     |> process.receive_forever
//     |> should.equal(Message(expected_exit_signal))
//     Normal
//   }
//
//   let spec = Spec(routine: routine, exit_trapper: Some(HandleExit))
//   assert Ok(_) = process.start_spec(spec)
//
//   process.async_send(linkee, Ping)
//   sleep(20)
// }
