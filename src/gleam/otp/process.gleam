import gleam/erlang/atom.{Atom}
import gleam/erlang/process.{Pid}
import gleam/dynamic.{Dynamic}

// TODO: implement decoder
// /// Attempt to parse a pid from some dynamic data.
// ///
// /// This may be useful if you receive a pid in a message from an Erlang process.
// pub external fn pid_from_dynamic(Dynamic) -> Result(Pid, List(DecodeError)) =
//   "gleam_otp_external" "pid_from_dynamic"

/// A message received when a linked process exits when the current process is
/// trapping exits.
///
pub type Exit {
  Exit(pid: Pid, reason: Dynamic)
}

pub type ExitReason {
  // The process is stopping due to normal and expected reasons. This is not
  // considered an error.
  Normal

  // The process is stopping as the supervision tree the process belongs to is
  // shutting down. This is not considered an error.
  Shutdown

  // The process is stopping due to an unexpected problem. This is considered
  // and error and should be reported and logged appropriately.
  Abnormal(Dynamic)
}

pub type Mode {
  Running
  Suspended
}

pub type DebugOption {
  NoDebug
}

pub external type DebugState

pub external fn debug_state(List(DebugOption)) -> DebugState =
  "sys" "debug_options"

pub type StatusInfo {
  StatusInfo(
    module: Atom,
    parent: Pid,
    mode: Mode,
    debug_state: DebugState,
    state: Dynamic,
  )
}

// // // TODO: document
// // // TODO: implement remaining messages
pub type SystemMessage {
  // //   // {replace_state, StateFn}
  // //   // {change_code, Mod, Vsn, Extra}
  // //   // {terminate, Reason}
  // //   // {debug, {log, Flag}}
  // //   // {debug, {trace, Flag}}
  // //   // {debug, {log_to_file, FileName}}
  // //   // {debug, {statistics, Flag}}
  // //   // {debug, no_debug}
  // //   // {debug, {install, {Func, FuncState}}}
  // //   // {debug, {install, {FuncId, Func, FuncState}}}
  // //   // {debug, {remove, FuncOrId}}
  Resume(fn() -> Nil)
  Suspend(fn() -> Nil)
  GetState(fn(Dynamic) -> Nil)
  GetStatus(fn(StatusInfo) -> Nil)
}
// type KillFlag {
//   Kill
// }

// external fn erlang_kill(to: Pid, because: KillFlag) -> Bool =
//   "erlang" "exit"

// // TODO: test
// /// Send an untrappable `kill` exit signal to the target process.
// ///
// /// See the documentation for the Erlang [`erlang:exit`][1] function for more
// /// information.
// ///
// /// [1]: https://erlang.org/doc/man/erlang.html#exit-1
// ///
// pub fn kill(pid: Pid) -> Nil {
//   erlang_kill(pid, Kill)
//   Nil
// }

// external fn erlang_send_exit(to: Pid, because: whatever) -> Bool =
//   "erlang" "exit"

// // TODO: test
// // TODO: refine exit reason. Maybe make dedicated functions for each type to
// // match kill
// /// Sends an exit signal to a process, indicating that that process is to shut
// /// down.
// ///
// /// See the [Erlang documentation][erl] for more information.
// /// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
// ///
// pub fn send_exit(to pid: Pid, because reason: whatever) -> Nil {
//   erlang_send_exit(pid, reason)
//   Nil
// }

// /// Start trapping exits within the current process and return a receiver.
// ///
// /// When not trapping exits if a linked process crashes an `Exit` message is
// /// sent over the channel. This is the normal behaviour before this function is
// /// called.
// ///
// /// When trapping exits (after this function is called) if a linked process
// /// crashes an `Exit` message is sent over the channel.
// ///
// pub external fn trap_exits() -> Receiver(Exit) =
//   "gleam_otp_external" "trap_exits"

// /// Stop trapping exits, causing any crashes in linked processes to also crash
// /// this process.
// ///
// /// See also the `trap_exits` function.
// ///
// pub external fn stop_trapping_exits() -> Nil =
//   "gleam_otp_external" "stop_trapping_exits"
