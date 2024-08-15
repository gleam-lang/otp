import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleam/otp/static_supervisor as sup
import gleeunit

pub fn main() {
  gleeunit.main()
}

@external(erlang, "gen_event", "start_link")
pub fn start_misc_process() -> Result(Pid, Dynamic)

pub fn start_one_for_one_test() {
  let assert Ok(pid) =
    sup.new(sup.OneForOne)
    |> sup.add(
      sup.worker_child("first", start_misc_process)
      |> sup.significant(False)
      |> sup.timeout(100),
    )
    |> sup.start_link

  let assert True = process.is_alive(pid)
  process.sleep(500)
  let assert True = process.is_alive(pid)
}

pub fn start_rest_for_one_test() {
  let assert Ok(pid) =
    sup.new(sup.RestForOne)
    |> sup.add(
      sup.worker_child("first", start_misc_process)
      |> sup.significant(False)
      |> sup.timeout(100),
    )
    |> sup.start_link

  let assert True = process.is_alive(pid)
  process.sleep(500)
  let assert True = process.is_alive(pid)
}

pub fn start_one_for_all_test() {
  let assert Ok(pid) =
    sup.new(sup.OneForAll)
    |> sup.add(
      sup.worker_child("first", start_misc_process)
      |> sup.significant(False)
      |> sup.timeout(100),
    )
    |> sup.start_link

  let assert True = process.is_alive(pid)
  process.sleep(500)
  let assert True = process.is_alive(pid)
}
