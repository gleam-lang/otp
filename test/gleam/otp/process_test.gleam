import gleam/otp/process
import gleam/expect
import gleam/result
import gleam/atom
import gleam/dynamic.{Dynamic}

external fn sleep(Int) -> Nil = "timer" "sleep"

pub fn link_dead_process_test() {
  let pid = process.spawn(fn(_) { Nil })
  sleep(20)
  pid
  |> process.link
  |> expect.equal(_, process.ProcessNotFound)
  process.unlink(pid)
}

pub fn link_live_process_test() {
  let pid = process.spawn(fn(_) { sleep(1000) })
  process.send_exit(pid, because: atom.create_from_string("normal"))
  pid
  |> process.link
  |> expect.equal(_, process.Linked)
  // TODO: check link exists
  process.unlink(pid)
}

pub fn is_alive_test() {
  let pid = process.spawn(fn(_) { sleep(1000) })
  pid
  |> process.is_alive
  |> expect.true
}

pub fn is_alive_dead_test() {
  let pid = process.spawn(fn(_) { Nil })
  sleep(20)
  pid
  |> process.is_alive
  |> expect.false
}

pub fn make_opaque_test() {
  let f = fn(add: fn(x) -> x) {
    fn(self) {
      self |> process.receive(_, 0) |> result.map(_, add)
    }
  }
  let float_pid = process.spawn(f(fn(x) { x +. 1. }))
  let int_pid = process.spawn(f(fn(x) { x + 1 }))
  // They can be compared now, they are the same type
  expect.false(
    process.make_opaque(float_pid) == process.make_opaque(int_pid)
  )
}

pub fn send_test() {
  let pid = process.spawn_link(fn(self) {
    self |> process.receive(_, 50) |> expect.equal(_, Ok(1))
    self |> process.receive(_, 50) |> expect.equal(_, Ok(2))
    self |> process.receive(_, 50) |> expect.equal(_, Ok(3))
  })
  process.send(pid, 1)
  process.send(pid, 2)
  process.send(pid, 3)
}

pub fn unsafe_downcast_send() {
  let f = fn(add: fn(x) -> x) {
    fn(self) {
      self |> process.receive(_, 0) |> result.map(_, add)
    }
  }
  let float_pid = process.spawn(f(fn(x) { x +. 1. }))
  let int_pid = process.spawn(f(fn(x) { x + 1 }))
  // They can be compared now, they are the same type
  let opaque_pid = process.make_opaque(int_pid)
  let fake_float_pid = process.unsafe_downcast(opaque_pid)
  expect.false(
    float_pid == fake_float_pid
  )
}

pub fn send_exit_test() {
  let pid = process.spawn(fn(_) { Nil })
  expect.true(process.is_alive(pid))
  process.send_exit(pid, atom.create_from_string("normal"))
  sleep(20)
  expect.false(process.is_alive(pid))
}

pub fn own_pid_test() {
  process.spawn(fn(self) {
    self
    |> process.own_pid
    |> process.make_opaque
    |> expect.equal(_, process.opaque_own_pid())
  })
  sleep(20)
}

pub fn opaque_receive_test() {
  let pid = process.spawn_link(fn(self) {
    self |> process.receive(_, 20) |> expect.equal(_, Ok(1))
    process.opaque_receive(20) |> expect.equal(_, Ok(dynamic.from("hi")))
  })
  process.send(pid, 1)
  pid |> process.make_opaque |> process.unsafe_downcast |> process.send(_, "hi")
  sleep(50)
}

type Exit {
  Exit(
    exited: process.Pid(process.UnknownMessage),
    reason: Dynamic,
  )
}

pub fn trap_exit_test() {
  let linkee = process.spawn(process.receive(_, 150))
  process.spawn(fn(self) {
    process.trap_exit(Exit)
    let expected_exit_signal = Exit(process.make_opaque(linkee), dynamic.from(1))
    self
    |> process.receive(_, 150)
    |> expect.equal(_, Ok(expected_exit_signal))
  })
  process.send(linkee, 1)
  sleep(20)
}
