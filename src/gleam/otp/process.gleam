// TODO: README
// TODO: monitor
// TODO: link
//
import gleam/atom
import gleam/result
import gleam/map.{Map}
import gleam/dynamic.{Dynamic}
import gleam/option.{None, Option}

external type DoNotLeak

pub external type SystemOk

pub fn system_ok() -> SystemOk {
  "ok"
  |> atom.from_string
  |> dynamic.from
  |> dynamic.unsafe_coerce
}

/// A Pid (or Process identifier) is a reference to an OTP process, which is a
/// lightweight thread that communicates by sending and receiving messages.
///
pub external type Pid

// TODO: document that this doesn't get type checked etc
// TODO: test
/// Send a message to a process.
///
/// Message sending is asynchronous and this function will likely return before
/// the message is handled by the receiving processes.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#send-2
///
pub external fn unsafe_send(to: Pid, msg: msg) -> msg =
  "erlang" "send"

// TODO: document
pub external type Reference

// TODO: document
pub external fn make_reference() -> Reference =
  "erlang" "make_ref"

// TODO: document
pub external fn self() -> Pid =
  "erlang" "self"

// TODO: document
pub opaque type Channel(msg) {
  Channel(pid: Pid, reference: Reference)
}

// TODO: document
pub fn make_channel() -> Channel(msg) {
  Channel(pid: self(), reference: make_reference())
}

// TODO: document
pub external fn channel_send(Channel(msg), msg) -> Channel(msg) =
  "gleam_otp_process_external" "channel_send"

pub external type ProcessMonitor

// TODO: test
// TODO: document
pub external fn monitor_process(Pid) -> ProcessMonitor =
  "gleam_otp_process_external" "monitor_process"

// TODO: test
// TODO: document
pub external fn demonitor_process(ProcessMonitor) -> Nil =
  "gleam_otp_process_external" "demonitor_process"

// TODO: document
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

// TODO: test
// TODO: document
pub external fn process_monitor_receive(
  ProcessMonitor,
  Int,
) -> Result(ProcessDown, Nil) =
  "gleam_otp_process_external" "process_monitor_receive"

pub external type Receiver(message)

pub external fn make_receiver() -> Receiver(message) =
  "gleam_otp_process_external" "make_receiver"

pub external fn run_receiver(Receiver(msg), Int) -> Result(msg, Nil) =
  "gleam_otp_process_external" "run_receiver"

pub external fn flush_receiver(Receiver(msg)) -> Int =
  "gleam_otp_process_external" "flush_receiver"

pub external fn add_channel(
  to: Receiver(b),
  add: Channel(a),
  mapping: fn(a) -> b,
) -> Receiver(b) =
  "gleam_otp_process_external" "add_channel"

pub external fn receieve_system_messages(Receiver(a)) -> Receiver(a) =
  "gleam_otp_process_external" "receive_system_messages"

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
// TODO: implement remaining messages
// TODO: better abstraction around this to make it more type safe
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
  GetStatus(Channel(Dynamic))
  Suspend(Channel(SystemOk))
  Resume(Channel(SystemOk))
  GetState(Channel(Dynamic))
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
  /// A regular message excepted by the process
  Message(message: msg)

  /// An OTP system message, for debugging or maintenance
  System(message: SystemMessage)
}

// TODO
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

pub external fn erlang_send_exit(to: Pid, because: ExitReason) -> Bool =
  "erlang" "exit"

// TODO: test
/// Sends an exit signal to a process, indicating that that process is to shut
/// down.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub fn send_exit(to pid: Pid, because reason: ExitReason) -> Nil {
  erlang_send_exit(pid, reason)
  Nil
}

// TODO: document
pub external fn start(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

// TODO: document
pub external fn start_unlinked(fn() -> anything) -> Pid =
  "erlang" "spawn"

// TODO: document
pub external fn receive_system_message_forever() -> SystemMessage =
  "gleam_otp_process_external" "receive_system_message_forever"

// TODO: document
pub fn receive(channel: Channel(msg), timeout: Int) -> Result(msg, Nil) {
  make_receiver()
  |> add_channel(channel, fn(x) { x })
  |> run_receiver(timeout)
}

// TODO: document
pub fn flush_channel(channel: Channel(msg)) -> Int {
  make_receiver()
  |> add_channel(channel, fn(x) { x })
  |> flush_receiver
}

// TODO: implement
// TODO: test
// TODO: document
pub fn channel_call(
  _channel: Channel(tuple(request, Channel(response))),
  _timeout: Int,
) -> Result(response, Nil) {
  todo("Channel call")
}
