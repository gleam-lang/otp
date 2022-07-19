import gleam/dynamic.{Dynamic}
import gleam/erlang/process.{Pid}

external type DoNotLeak

/// Get the state of a given OTP compatible process. This function is only
/// intended for debugging.
///
/// For more information see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/man/sys.html#get_state-1
///
pub external fn get_state(from: Pid) -> Dynamic =
  "sys" "get_state"

external fn erl_suspend(Pid) -> DoNotLeak =
  "sys" "suspend"

/// Request an OTP compatible process to suspend, causing it to only handle
/// system messages.
///
/// For more information see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/man/sys.html#suspend-1
///
pub fn suspend(pid: Pid) -> Nil {
  erl_suspend(pid)
  Nil
}

external fn erl_resume(from: Pid) -> DoNotLeak =
  "sys" "resume"

/// Request a suspended OTP compatible process to result, causing it to handle
/// all messages rather than only system messages.
///
/// For more information see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/man/sys.html#resume-1
///
pub fn resume(pid: Pid) -> Nil {
  erl_resume(pid)
  Nil
}
