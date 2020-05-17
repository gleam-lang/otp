import gleam/otp/process.{Message}
import gleam/should
import gleam/result
import gleam/atom
import gleam/dynamic.{Dynamic}

external fn sleep(Int) -> Nil =
  "timer" "sleep"

pub fn is_alive_test() {
  assert Ok(pid) = process.async_start(fn(_) { sleep(1000) })
  pid
  |> process.is_alive
  |> should.equal(True)
}

pub fn is_alive_dead_test() {
  assert Ok(pid) = process.async_start(fn(_) { Nil })
  sleep(20)
  pid
  |> process.is_alive
  |> should.equal(False)
}

pub fn make_opaque_test() {
  let f = fn(handle: fn(x) -> x) {
    fn(self) {
      assert Ok(Message(msg)) = process.receive(self, 1000)
      handle(msg)
    }
  }
  assert Ok(float_pid) = process.async_start(f(fn(x) { x +. 1. }))
  assert Ok(int_pid) = process.async_start(f(fn(x) { x + 1 }))
  // They can be compared now, they are the same type
  should.not_equal(process.make_opaque(float_pid), process.make_opaque(int_pid))
}

pub fn send_test() {
  assert Ok(
    pid,
  ) = process.async_start(
    fn(self) {
      self
      |> process.receive(50)
      |> should.equal(Ok(Message(1)))
      self
      |> process.receive(50)
      |> should.equal(Ok(Message(2)))
      self
      |> process.receive(50)
      |> should.equal(Ok(Message(3)))
    },
  )
  process.async_send(pid, 1)
  process.async_send(pid, 2)
  process.async_send(pid, 3)
}

pub fn unsafe_downcast_send() {
  let f = fn(handle: fn(x) -> x) {
    fn(self) {
      assert Ok(Message(msg)) = process.receive(self, 1000)
      handle(msg)
    }
  }
  assert Ok(float_pid) = process.async_start(f(fn(x) { x +. 1. }))
  assert Ok(int_pid) = process.async_start(f(fn(x) { x + 1 }))
  let opaque_pid = process.make_opaque(int_pid)
  let fake_float_pid = process.unsafe_downcast(opaque_pid)
  // They can be compared now, they are the same type
  should.equal(float_pid, fake_float_pid)
}

// fn loop(self: process.Self(a)) -> never {
//   sleep(5)
//   loop(self)
// }
// pub fn send_exit_test() {
//   assert Ok(pid) = process.async_start(loop)
//   should.equal(process.is_alive(pid), True)
//   process.send_exit(pid, atom.create_from_string("normal"))
//   sleep(20)
//   should.equal(process.is_alive(pid), False)
// }
pub fn own_pid_test() {
  let _ = process.async_start(
    fn(self) {
      self
      |> process.own_pid
      |> process.make_opaque
      |> should.equal(process.opaque_own_pid())
    },
  )
  sleep(20)
}
