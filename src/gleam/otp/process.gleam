// TODO: README
// TODO: link
// TODO: timers
//
import gleam/atom
import gleam/result
import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/option.{None}
import gleam/otp/port.{Port}
import gleam/function

/// A Pid (or Process identifier) is a reference to an OTP process, which is a
/// lightweight thread that communicates by sending and receiving messages.
///
pub external type Pid

/// Send a bare message to a process. These bare messages are not not commonly
/// used in Gleam as they are not typed checked, see the Channel type for a
/// type safe and more ergonomic way of sending messages to processes. This
/// function may still be useful for sending messages to processes implemented
/// in Erlang or other BEAM languages.
///
/// Message sending is asynchronous and this function will likely return before
/// the message is handled by the receiving processes.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#send-2
///
pub external fn untyped_send(to: Pid, msg: msg) -> msg =
  "erlang" "send"

// TODO: document
pub external type Reference

// TODO: document
pub external fn new_reference() -> Reference =
  "erlang" "make_ref"

// TODO: document
pub external fn self() -> Pid =
  "erlang" "self"

// TODO: document
pub opaque type Channel(msg) {
  Channel(
    pid: Pid,
    reference: Reference,
    send: fn(Dynamic) -> Dynamic,
    build_message: fn(msg) -> Dynamic,
  )
}

external fn open_channel(Reference) -> Nil =
  "gleam_otp_external" "open_channel"

external fn close_channel_reference(Reference) -> Nil =
  "gleam_otp_external" "close_channel"

// TODO: document
// TODO: test
pub fn close_channel(channel: Channel(msg)) -> Nil {
  close_channel_reference(channel.reference)
}

// TODO: document
pub fn pid(channel: Channel(msg)) -> Pid {
  channel.pid
}

// TODO: document
// TODO: document that `close_channel` should be called
pub fn new_channel() -> Channel(msg) {
  let ref = new_reference()
  let self = self()
  open_channel(ref)
  Channel(
    pid: self,
    reference: ref,
    send: untyped_send(self, _),
    build_message: fn(msg) { dynamic.from(tuple(ref, msg)) },
  )
}

// TODO: document
pub fn send(channel: Channel(message), message: message) -> Channel(message) {
  message
  |> channel.build_message
  |> channel.send
  channel
}

/// Create a channel that immediately discards any messages sent on it.
///
/// This may be useful for wrapping Erlang processes which do not use channels,
/// or other situations in which you need to return a channel but do not have
/// one available.
///
pub fn null_channel(pid: Pid) -> Channel(msg) {
  Channel(
    pid: pid,
    reference: new_reference(),
    build_message: dynamic.from,
    send: function.identity,
  )
}

type ProcessMonitorFlag {
  Process
}

pub opaque type ProcessMonitor {
  ProcessMonitor(reference: Reference)
}

external fn erlang_monitor_process(ProcessMonitorFlag, Pid) -> Reference =
  "erlang" "monitor"

// TODO: document
pub fn monitor_process(pid: Pid) -> ProcessMonitor {
  ProcessMonitor(reference: erlang_monitor_process(Process, pid))
}

type PortMonitorFlag {
  Port
}

pub opaque type PortMonitor {
  PortMonitor(reference: Reference)
}

external fn erlang_port_monitor(PortMonitorFlag, Port) -> Reference =
  "erlang" "monitor"

// TODO: test
// TODO: document
pub fn monitor_port(port: Port) -> PortMonitor {
  PortMonitor(reference: erlang_port_monitor(Port, port))
}

type DemonitorOption {
  Flush
}

external fn erlang_demonitor(Reference, List(DemonitorOption)) -> Bool =
  "erlang" "demonitor"

// TODO: document
pub fn demonitor_process(monitor: ProcessMonitor) -> Nil {
  erlang_demonitor(monitor.reference, [Flush])
  Nil
}

// TODO: test
// TODO: document
pub fn demonitor_port(monitor: PortMonitor) -> Nil {
  erlang_demonitor(monitor.reference, [Flush])
  Nil
}

// TODO: document
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

// TODO: document
pub type PortDown {
  PortDown(port: Port, reason: Dynamic)
}

// TODO: document
pub external type Receiver(message)

// TODO: document
pub external fn new_receiver() -> Receiver(message) =
  "gleam_otp_external" "new_receiver"

// TODO: document
pub external fn run_receiver(Receiver(msg)) -> Result(msg, Nil) =
  "gleam_otp_external" "run_receiver"

// TODO: document
// Be careful!
pub external fn run_receiver_forever(Receiver(msg)) -> msg =
  "gleam_otp_external" "run_receiver_forever"

// TODO: document
pub external fn flush_receiver(Receiver(msg)) -> Int =
  "gleam_otp_external" "flush_receiver"

// TODO: document
pub external fn include_channel(
  to: Receiver(b),
  add: Channel(a),
  mapping: fn(a) -> b,
) -> Receiver(b) =
  "gleam_otp_external" "include_channel"

// TODO: document
pub external fn include_process_monitor(
  to: Receiver(b),
  add: ProcessMonitor,
  mapping: fn(ProcessDown) -> b,
) -> Receiver(b) =
  "gleam_otp_external" "include_process_monitor"

pub type Exit {
  Exit(pid: Pid, reason: Dynamic)
}

// TODO: test
// TODO: document
pub external fn include_process_exit(
  to: Receiver(b),
  add: Pid,
  mapping: fn(Exit) -> b,
) -> Receiver(b) =
  "gleam_otp_external" "include_process_exit"

// TODO: test
// TODO: document
pub external fn include_all_exits(
  to: Receiver(b),
  mapping: fn(Exit) -> b,
) -> Receiver(b) =
  "gleam_otp_external" "include_all_exits"

// TODO: document
pub external fn include_port_monitor(
  to: Receiver(b),
  add: PortMonitor,
  mapping: fn(PortDown) -> b,
) -> Receiver(b) =
  "gleam_otp_external" "include_port_monitor"

// TODO: document
pub external fn set_timeout(Receiver(a), Int) -> Receiver(a) =
  "gleam_otp_external" "set_timeout"

// TODO: document
pub external fn flush_other(Receiver(a), Bool) -> Receiver(a) =
  "gleam_otp_external" "flush_other"

// TODO: test
// TODO: document
pub external fn remove_timeout(Receiver(a)) -> Receiver(a) =
  "gleam_otp_external" "remove_timeout"

// TODO: test flushing
// TODO: document
pub external fn include_system(
  Receiver(a),
  fn(SystemMessage) -> a,
) -> Receiver(a) =
  "gleam_otp_external" "include_system"

// TODO: test
// TODO: document
pub external fn include_all(Receiver(a), fn(Dynamic) -> a) -> Receiver(a) =
  "gleam_otp_external" "include_all"

// TODO: test
// TODO: document
pub external fn include_bare(Receiver(a), fn(Dynamic) -> a) -> Receiver(a) =
  "gleam_otp_external" "include_bare"

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
    mod: Atom,
    parent: Pid,
    mode: Mode,
    debug_state: DebugState,
    state: Dynamic,
  )
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
  GetStatus(Channel(StatusInfo))
  Suspend(Channel(Nil))
  Resume(Channel(Nil))
  GetState(Channel(Dynamic))
}

/// Check to see whether the process for a given Pid is alive.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
///
pub external fn is_alive(Pid) -> Bool =
  "erlang" "is_process_alive"

type KillFlag {
  Kill
}

external fn erlang_kill(to: Pid, because: KillFlag) -> Bool =
  "erlang" "exit"

// TODO: document
// TODO: test
pub fn kill(pid: Pid) -> Bool {
  erlang_kill(pid, Kill)
}

external fn erlang_send_exit(to: Pid, because: whatever) -> Bool =
  "erlang" "exit"

// TODO: test
// TODO: refine exit reason. Maybe make dedicated functions for each type to
// match kill
/// Sends an exit signal to a process, indicating that that process is to shut
/// down.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub fn send_exit(to pid: Pid, because reason: whatever) -> Nil {
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
  new_receiver()
  |> include_channel(channel, fn(x) { x })
  |> set_timeout(timeout)
  |> run_receiver
}

// TODO: document
pub fn flush(channel: Channel(msg)) -> Int {
  new_receiver()
  |> include_channel(channel, fn(x) { x })
  |> flush_receiver
}

// TODO: document
pub type CallError(msg) {
  // TODO: document
  CalleeDown(reason: Dynamic)
  // TODO: document
  CallTimeout
}

fn process_down_to_call_error(down: ProcessDown) -> Result(a, CallError(a)) {
  Error(CalleeDown(reason: down.reason))
}

// TODO: test error paths
// TODO: document
// This function is based off of Erlang's gen:do_call/4.
pub fn try_call(
  channel: Channel(request),
  make_request: fn(Channel(response)) -> request,
  timeout: Int,
) -> Result(response, CallError(response)) {
  let reply_channel = new_channel()

  // Monitor the callee process so we can tell if it goes down (meaning we
  // won't get a reply)
  let monitor =
    channel
    |> pid
    |> monitor_process

  // Send the request to the process over the channel
  send(channel, make_request(reply_channel))

  // Await a reply or handle failure modes (timeout, process down, etc)
  let res =
    new_receiver()
    |> include_channel(reply_channel, Ok)
    |> include_process_monitor(monitor, process_down_to_call_error)
    |> set_timeout(timeout)
    |> run_receiver

  // Demonitor the process as we're done
  demonitor_process(monitor)
  close_channel(channel)

  // Prepare an appropriate error (if present) for the caller
  case res {
    Error(Nil) -> Error(CallTimeout)
    Ok(res) -> res
  }
}

// TODO: test error paths
// TODO: document
pub fn call(
  channel: Channel(request),
  make_request: fn(Channel(response)) -> request,
  timeout: Int,
) -> response {
  assert Ok(resp) = try_call(channel, make_request, timeout)
  resp
}

type MessageQueueLenFlag {
  MessageQueueLen
}

external fn process_info_message_queue_length(
  Pid,
  MessageQueueLenFlag,
) -> tuple(Atom, Int) =
  "erlang" "process_info"

// TODO: document
pub fn message_queue_size(pid: Pid) -> Int {
  process_info_message_queue_length(pid, MessageQueueLen).1
}

type TrapExit {
  TrapExit
}

external fn erlang_trap_exit(TrapExit, Bool) -> Bool =
  "erlang" "process_flag"

// TODO: test
// TODO: document
pub fn trap_exit(bool: Bool) -> Nil {
  erlang_trap_exit(TrapExit, bool)
  Nil
}

// TODO: document
pub fn wrap_channel(
  channel: Channel(a),
  with preprocessor: fn(b) -> a,
) -> Channel(b) {
  Channel(
    pid: channel.pid,
    send: channel.send,
    reference: channel.reference,
    build_message: fn(b) {
      b
      |> preprocessor
      |> channel.build_message
    },
  )
}
