// TODO: README
// TODO: monitor
// TODO: link
//
import gleam/atom
import gleam/result
import gleam/dynamic.{Dynamic}
import gleam/option.{None, Option}

external type DoNotLeak

/// A Pid (or Process identifier) is a reference to an OTP process, which is a
/// lightweight thread that communicates by sending and receiving messages.
///
pub external type Pid

pub type ExitReason {
  // The process is stopping due to normal and expected reasons. This is not
  // considered an error.
  Normal

  // The process is stopping as the supervision tree the process belongs to is
  // shutting down. This is not considered an error.
  Shutdown

  // The process is stopping due to an unexpected problem. This is considered
  // and error and should be reported and logged appropriately.
  Abnormal(String)
}

// TODO: document
pub external type Ref

// TODO: document
// Special thanks to Peter Saxton for the idea of a typed From value
pub opaque type From(reply) {
  From(reply: fn(reply) -> Nil)
}

// TODO: document
// TODO: test
pub fn wrap_from(from: From(a), with adapter: fn(b) -> a) -> From(b) {
  From(reply: fn(b) { from.reply(adapter(b)) })
}

// TODO: document
pub fn reply(to caller: From(reply), with payload: reply) -> Nil {
  caller.reply(payload)
}

// TODO: document
// TODO: implement remaining messages
pub type SystemMessage {
  // {replace_state, StateFn}
  // {change_code, Mod, Vsn, Extra}
  // {terminate, Reason}
  // {debug, {log, Flag}}
  // {debug, {trace, Flag}}
  // {debug, {log_to_file, FileName}}
  // {debug, {statistics, Flag}}
  // {debug, no_debug}
  // {debug, {install, {Func, FuncState}}}
  // {debug, {install, {FuncId, Func, FuncState}}}
  // {debug, {remove, FuncOrId}}
  GetStatus(From(Dynamic))
  Suspend(From(Nil))
  Resume(From(Nil))
  GetState(From(Dynamic))
}

pub type DebugOption {
  NoDebug
}

pub external type DebugState

pub external fn debug_state(List(DebugOption)) -> DebugState =
  "sys" "debug_options"

pub type StartResult =
  Result(Pid, ExitReason)

// TODO: document
pub type Message(msg) {
  Message(
    /// A regular message excepted by the process
    message: msg,
  )

  System(
    /// An OTP system message, for debugging or maintenance
    message: SystemMessage,
  )
}

pub external fn erl_async_send(to: Pid, msg: msg) -> msg =
  "erlang" "send"

// TODO
// Send a message to a process.
//
// Message sending is asynchronous and this function will likely return before
// the message is handled by the receiving processes.
//
// See the [Erlang documentation][erl] for more information.
// [erl]: http://erlang.org/doc/man/erlang.html#send-2
//
//pub fn async_send(to receiever: Pid(msg), msg msg: msg) -> Nil {
//  erl_async_send(receiever, msg)
//  Nil
//}
// TODO: document
//pub external fn sync_send(
//  to: Pid(msg),
//  message: fn(From(reply)) -> msg,
//  timeout: Int,
//) -> reply =
//  "gleam_otp_process_external" "sync_send"
/// Check to see whether the process for a given Pid is alive.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
///
pub external fn is_alive(Pid) -> Bool =
  "erlang" "is_process_alive"

// TODO: test
// TODO: document
pub external fn self() -> Pid =
  "erlang" "self"

// TODO: test
/// Sends an exit signal to a process, indicating that that process is to shut
/// down.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub external fn send_exit(to: Pid, because: reason) -> Nil =
  "gleam_otp_process_external" "send_exit"

// TODO: document
pub external fn start(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

// TODO: document
pub external fn receive_system_message_forever() -> SystemMessage =
  "gleam_otp_process_external" "receive_system_message_forever"
