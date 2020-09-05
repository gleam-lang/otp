// TODO: README
// TODO: link
// TODO: port monitor
// TODO: flush_other ?
// TODO: wrap_sender
// TODO: wrap_receiver
// TODO: receive system messages
//
import gleam/atom
import gleam/result
import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
import gleam/option.{None}
import gleam/otp/port.{Port}
import gleam/function
import gleam/option.{None, Option, Some}

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
pub type Sender(msg) {
  Sender(pid: Pid, reference: Reference, prepare: Option(fn(msg) -> Dynamic))
}

// TODO: document
pub external type Receiver(msg)

pub external fn new_receiver(Reference) -> Receiver(msg) =
  "gleam_otp_external" "new_receiver"

// TODO: document
// TODO: test
pub fn new_channel() -> tuple(Sender(msg), Receiver(msg)) {
  let self = self()
  let reference = new_reference()
  let prepare = fn(msg) { dynamic.from(tuple(reference, msg)) }
  let sender = Sender(pid: self, reference: reference, prepare: Some(prepare))
  let receiver = new_receiver(reference)
  tuple(sender, receiver)
}

// TODO: document
// TODO: test
pub external fn close_channels(Receiver(msg)) -> Nil =
  "gleam_otp_external" "close_channels"

// TODO: document
pub fn pid(sender: Sender(msg)) -> Pid {
  sender.pid
}

// TODO: document
pub fn send(sender: Sender(msg), message: msg) -> Sender(msg) {
  case sender.prepare {
    Some(prepare) -> {
      message
      |> prepare
      |> untyped_send(sender.pid, _)
      sender
    }
    None -> sender
  }
}

/// Create a sender that immediately discards any messages sent on it.
///
/// This may be useful for wrapping Erlang processes which do not use channels,
/// or other situations in which you need to return a sender but do not have
/// one available.
///
pub fn null_sender(pid: Pid) -> Sender(msg) {
  Sender(pid: pid, reference: new_reference(), prepare: None)
}

type ProcessMonitorFlag {
  Process
}

external fn erlang_monitor_process(ProcessMonitorFlag, Pid) -> Reference =
  "erlang" "monitor"

// TODO: document
// TODO: test
// TODO: test closing
pub fn monitor_process(pid: Pid) -> Receiver(ProcessDown) {
  let reference = erlang_monitor_process(Process, pid)
  new_receiver(reference)
}

type PortMonitorFlag {
  Port
}

external fn erlang_port_monitor(PortMonitorFlag, Port) -> Reference =
  "erlang" "monitor"

// // TODO: test
// // TODO: document
// pub fn monitor_port(port: Port) -> PortMonitor {
//   PortMonitor(reference: erlang_port_monitor(Port, port))
// }
//
// TODO: document
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

// // TODO: document
// pub type PortDown {
//   PortDown(port: Port, reason: Dynamic)
// }
//
// TODO: document
// Be careful!
pub external fn receive_forever(Receiver(msg)) -> msg =
  "gleam_otp_external" "run_receiver_forever"

// TODO: document
pub external fn flush(Receiver(msg)) -> Int =
  "gleam_otp_external" "flush_receiver"

// TODO: document
// TODO: test
pub external fn merge_receiver(Receiver(a), Receiver(a)) -> Receiver(a) =
  "gleam_otp_external" "merge_receiver"

pub type Exit {
  Exit(pid: Pid, reason: Dynamic)
}

// TODO: test
// TODO: document
pub external fn bare_message_receiver() -> Receiver(Dynamic) =
  "gleam_otp_external" "bare_message_receiver"

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
  GetStatus(Sender(StatusInfo))
  Suspend(Sender(Nil))
  Resume(Sender(Nil))
  GetState(Sender(Dynamic))
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
pub external fn receive(
  receiver: Receiver(msg),
  timeout: Int,
) -> Result(msg, Nil) =
  "gleam_otp_external" "run_receiver"

// // TODO: document
// pub type CallError(msg) {
//   // TODO: document
//   CalleeDown(reason: Dynamic)
//   // TODO: document
//   CallTimeout
// }
//
// fn process_down_to_call_error(down: ProcessDown) -> Result(a, CallError(a)) {
//   Error(CalleeDown(reason: down.reason))
// }
//
// // TODO: test error paths
// // TODO: document
// // This function is based off of Erlang's gen:do_call/4.
// pub fn try_call(
//   channel: Channel(request),
//   make_request: fn(Channel(response)) -> request,
//   timeout: Int,
// ) -> Result(response, CallError(response)) {
//   let reply_channel = old_new_channel()
//
//   // Monitor the callee process so we can tell if it goes down (meaning we
//   // won't get a reply)
//   let monitor =
//     channel
//     |> pid
//     |> monitor_process
//
//   // Send the request to the process over the channel
//   send(channel, make_request(reply_channel))
//
//   // Await a reply or handle failure modes (timeout, process down, etc)
//   let res =
//     new_receiver()
//     |> include_channel(reply_channel, Ok)
//     |> include_process_monitor(monitor, process_down_to_call_error)
//     |> set_timeout(timeout)
//     |> run_receiver
//
//   // Demonitor the process as we're done
//   demonitor_process(monitor)
//   close_channel_old(channel)
//
//   // Prepare an appropriate error (if present) for the caller
//   case res {
//     Error(Nil) -> Error(CallTimeout)
//     Ok(res) -> res
//   }
// }
//
// // TODO: test error paths
// // TODO: document
// pub fn call(
//   channel: Channel(request),
//   make_request: fn(Channel(response)) -> request,
//   timeout: Int,
// ) -> response {
//   assert Ok(resp) = try_call(channel, make_request, timeout)
//   resp
// }
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

// TODO: document
pub external fn trap_exits() -> Receiver(Exit) =
  "gleam_otp_external" "trap_exits"

pub external type Timer

external fn erlang_send_after(Int, Pid, msg) -> Timer =
  "erlang" "send_after"

external fn fake_timer() -> Timer =
  "erlang" "make_ref"

// TODO: document
pub fn send_after(sender: Sender(msg), delay: Int, message: msg) -> Timer {
  case sender.prepare {
    Some(prepare) -> erlang_send_after(delay, sender.pid, prepare(message))
    None -> fake_timer()
  }
}

external fn erlang_cancel_timer(Timer) -> Dynamic =
  "erlang" "cancel_timer"

pub type Cancelled {
  TimerNotFound
  Cancelled(time_remaining: Int)
}

// TODO: document
pub fn cancel_timer(timer: Timer) -> Cancelled {
  case dynamic.int(erlang_cancel_timer(timer)) {
    Ok(i) -> Cancelled(i)
    Error(_) -> TimerNotFound
  }
}
