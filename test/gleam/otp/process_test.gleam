import gleam/otp/process
import gleam/expect
import gleam/result
import gleam/atom

external fn sleep(Int) -> Nil = "timer" "sleep"

pub fn link_dead_process_test() {
  let pid = process.spawn(fn(_) { Nil }, [])
  sleep(20)
  pid
  |> process.link
  |> expect.equal(_, process.ProcessNotFound)
  process.unlink(pid)
}

pub fn link_live_process_test() {
  let pid = process.spawn(fn(_) { sleep(1000) }, [])
  process.send_exit(pid, because: atom.create_from_string("normal"))
  pid
  |> process.link
  |> expect.equal(_, process.Linked)
  // TODO: check link exists
  process.unlink(pid)
}

pub fn is_alive_test() {
  let pid = process.spawn(fn(_) { sleep(1000) }, [])
  pid
  |> process.is_alive
  |> expect.true
}

pub fn is_alive_dead_test() {
  let pid = process.spawn(fn(_) { Nil }, [])
  sleep(20)
  pid
  |> process.is_alive
  |> expect.false
}

pub fn make_opaque_test() {
  let f = fn(add: fn(x) -> x) {
    fn(self) {
      self |> process.receive_(_, 0) |> result.map(_, add)
      Nil
    }
  }
  let float_pid = process.spawn(f(fn(x) { x +. 1. }), [])
  let int_pid = process.spawn(f(fn(x) { x + 1 }), [])
  // They can be compared now, they are the same type
  expect.false(
    process.make_opaque(float_pid) == process.make_opaque(int_pid)
  )
}

pub fn send_test() {
  let pid = process.spawn(fn(self) {
    self |> process.receive_(_, 50) |> expect.equal(_, Ok(1))
    self |> process.receive_(_, 50) |> expect.equal(_, Ok(2))
    self |> process.receive_(_, 50) |> expect.equal(_, Ok(3))
    Nil
  }, [process.Link])
  process.send(pid, 1)
  process.send(pid, 2)
  process.send(pid, 3)
}
