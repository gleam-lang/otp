import gleam/dynamic.{Dynamic}
import gleam/otp/process.{Pid}

external type DoNotLeak

// TODO: document
pub external fn get_state(from: Pid) -> Dynamic =
  "sys" "get_state"

external fn erl_suspend(Pid) -> DoNotLeak =
  "sys" "suspend"

// TODO: document
pub fn suspend(pid: Pid) -> Nil {
  erl_suspend(pid)
  Nil
}

external fn erl_resume(from: Pid) -> DoNotLeak =
  "sys" "resume"

// TODO: document
pub fn resume(pid: Pid) -> Nil {
  erl_resume(pid)
  Nil
}
