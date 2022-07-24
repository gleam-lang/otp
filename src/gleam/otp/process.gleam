import gleam/erlang/atom.{Atom}
import gleam/erlang/process.{Pid, Subject}
import gleam/dynamic.{Dynamic}

// import gleam/otp/port
// import gleam/function
// import gleam/option.{None, Option, Some}
// import gleam/erlang.{Reference}

// /// Attempt to parse a pid from some dynamic data.
// ///
// /// This may be useful if you receive a pid in a message from an Erlang process.
// pub external fn pid_from_dynamic(Dynamic) -> Result(Pid, List(DecodeError)) =
//   "gleam_otp_external" "pid_from_dynamic"

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
  GetStatus(Subject(StatusInfo))
  Suspend(Subject(Nil))
  Resume(Subject(Nil))
  GetState(Subject(Dynamic))
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

// pub external type Timer

// external fn erlang_send_after(Int, Pid, msg) -> Timer =
//   "erlang" "send_after"

// external fn fake_timer() -> Timer =
//   "erlang" "make_ref"

// /// Send a message over a channel after a specified timeout.
// ///
// pub fn send_after(sender: Sender(msg), delay: Int, message: msg) -> Timer {
//   case sender.prepare {
//     Some(prepare) -> erlang_send_after(delay, sender.pid, prepare(message))
//     None -> fake_timer()
//   }
// }

// external fn erlang_cancel_timer(Timer) -> Dynamic =
//   "erlang" "cancel_timer"

// /// Values returned when a timer is cancelled.
// ///
// pub type Cancelled {
//   /// The timer could not be found. It probably has already triggered.
//   ///
//   TimerNotFound

//   /// The timer was found and cancelled before it triggered.
//   ///
//   Cancelled(time_remaining: Int)
// }

// /// Cancel a given timer, causing it not to trigger if it has not done already.
// ///
// pub fn cancel_timer(timer: Timer) -> Cancelled {
//   case dynamic.int(erlang_cancel_timer(timer)) {
//     Ok(i) -> Cancelled(i)
//     Error(_) -> TimerNotFound
//   }
// }

// external fn erlang_link(Pid) -> Bool =
//   "erlang" "link"

// // TODO: test
// /// Creates a link between the calling process and another process.
// ///
// /// When a process crashes any linked processes will also crash. This is useful
// /// to ensure that groups of processes that depend on each other all either
// /// succeed or fail together.
// ///
// /// See the `gleam/otp/supervisor` module and the `trap_exits` function for
// /// mechanisms for handling process crashes.
// ///
// pub fn link(pid: Pid) -> Nil {
//   erlang_link(pid)
//   Nil
// }

// external fn erlang_unlink(pid: Pid) -> Bool =
//   "erlang" "link"

// // TODO: test
// /// Removes any existing link between the caller process and the target process.
// ///
// pub fn unlink(pid: Pid) -> Nil {
//   erlang_unlink(pid)
//   Nil
// }
