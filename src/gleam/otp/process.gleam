// TODO: README
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
pub fn pid(channel: Channel(msg)) -> Pid {
  channel.pid
}

// TODO: document
pub fn make_channel() -> Channel(msg) {
  Channel(pid: self(), reference: make_reference())
}

// TODO: document
pub fn send(channel: Channel(msg), msg: msg) -> Channel(msg) {
  unsafe_send(channel.pid, tuple(channel.reference, msg))
  channel
}

type ProcessMonitorFlag {
  Process
}

external fn erlang_monitor_process(ProcessMonitorFlag, Pid) -> Reference =
  "erlang" "monitor"

// TODO: test
// TODO: document
pub fn monitor_process(pid: Pid) -> Channel(ProcessDown) {
  Channel(pid: pid, reference: erlang_monitor_process(Process, pid))
}

type DemonitorOption {
  Flush
}

external fn erlang_demonitor_process(Reference, List(DemonitorOption)) -> Bool =
  "erlang" "demonitor"

// TODO: test
// TODO: document
pub fn demonitor_process(monitor_channel: Channel(ProcessDown)) -> Nil {
  erlang_demonitor_process(monitor_channel.reference, [Flush])
  Nil
}

// TODO: document
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

// TODO: document
pub external type Receiver(message)

// TODO: document
pub external fn make_receiver() -> Receiver(message) =
  "gleam_otp_process_external" "make_receiver"

// TODO: document
pub external fn run_receiver(Receiver(msg)) -> Result(msg, Nil) =
  "gleam_otp_process_external" "run_receiver"

// TODO: document
pub external fn flush_receiver(Receiver(msg)) -> Int =
  "gleam_otp_process_external" "flush_receiver"

// TODO: document
pub external fn include(
  to: Receiver(b),
  add: Channel(a),
  mapping: fn(a) -> b,
) -> Receiver(b) =
  "gleam_otp_process_external" "include_channel"

// TODO: test
// TODO: document
pub external fn set_timeout(Receiver(a), Int) -> Receiver(a) =
  "gleam_otp_process_external" "set_timeout"

// TODO: test
// TODO: document
pub external fn remove_timeout(Receiver(a)) -> Receiver(a) =
  "gleam_otp_process_external" "remove_timeout"

// TODO: test for receiver flushing
// TODO: document
pub external fn include_system(
  Receiver(a),
  fn(SystemMessage) -> a,
) -> Receiver(a) =
  "gleam_otp_process_external" "include_system"

// TODO: test
// TODO: document
pub external fn include_bare(Receiver(a), fn(Dynamic) -> a) -> Receiver(a) =
  "gleam_otp_process_external" "include_bare"

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
pub fn receive(channel: Channel(msg), timeout: Int) -> Result(msg, Nil) {
  make_receiver()
  |> include(channel, fn(x) { x })
  |> set_timeout(timeout)
  |> run_receiver
}

// TODO: document
pub fn flush(channel: Channel(msg)) -> Int {
  make_receiver()
  |> include(channel, fn(x) { x })
  |> flush_receiver
}

// TODO: document
pub type CallError(msg) {
  // TODO: document
  CalleeDown(reason: Dynamic)
  // TODO: document (i.e. the receiver is so that you can clean up the message
  // if it arrives later, to avoid a memory leak)
  CallTimeout(reciever: Channel(msg))
}

fn process_down_to_call_error(down: ProcessDown) -> Result(a, CallError(a)) {
  Error(CalleeDown(reason: down.reason))
}

// TODO: test error paths
// TODO: document
// This function is based off of Erlang's gen:do_call/4.
pub fn try_call(
  channel: Channel(tuple(request, Channel(response))),
  request: request,
  timeout: Int,
) -> Result(response, CallError(response)) {
  let reply_channel = make_channel()

  // Monitor the callee process so we can tell if it goes down (meaning we
  // won't get a reply)
  let monitor_channel = channel
    |> pid
    |> monitor_process

  // Send the request to the process over the channel
  send(channel, tuple(request, reply_channel))

  // Await a reply or handle failure modes (timeout, process down, etc)
  let res = make_receiver()
    |> include(reply_channel, Ok)
    |> include(monitor_channel, process_down_to_call_error)
    |> set_timeout(timeout)
    |> run_receiver

  // Demonitor the process as we're done
  demonitor_process(monitor_channel)

  // Prepare an appropriate error (if present) for the caller
  case res {
    Error(Nil) -> Error(CallTimeout(reply_channel))
    Ok(res) -> res
  }
}

// TODO: test error paths
// TODO: document
pub fn call(
  channel: Channel(tuple(request, Channel(response))),
  request: request,
  timeout: Int,
) -> response {
  assert Ok(resp) = try_call(channel, request, timeout)
  resp
}
