import gleam/dynamic.{Dynamic}
import gleam/otp/process.{Pid}

external type DoNotLeak

// TODO: document
pub external fn get_state(from: Pid(a)) -> Dynamic =
  "sys" "get_state"

external fn erl_suspend(Pid(a)) -> DoNotLeak =
  "sys" "suspend"

// TODO: test
// TODO: document
pub fn suspend(pid: Pid(a)) -> Nil {
  erl_suspend(pid)
  Nil
}

external fn erl_resume(from: Pid(a)) -> DoNotLeak =
  "sys" "resume"

// TODO: test
// TODO: document
pub fn resume(pid: Pid(a)) -> Nil {
  erl_resume(pid)
  Nil
}
