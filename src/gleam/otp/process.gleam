import gleam/atom
import gleam/result
import gleam/atom.{Atom}
import gleam/dynamic.{Dynamic}
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
/// Message handling is asynchronous and this function will likely return before
/// the message is handled by the receiving processes.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#send-2
///
pub external fn untyped_send(to: Pid, msg: msg) -> msg =
  "erlang" "send"

/// A reference is a special value where each new one is unique. For more
/// information references see the [Erlang documentation][1].
///
/// [1]: https://erlang.org/doc/efficiency_guide/advanced.html#unique_references
///
pub external type Reference

/// Create a new reference. The reference is unique among the currently
/// connected nodes in the Erlang cluster.
///
pub external fn new_reference() -> Reference =
  "erlang" "make_ref"

/// Get the Pid of the process that calls the function.
///
pub external fn self() -> Pid =
  "erlang" "self"

/// A sender is one end of a channel, it allows one or more processes to send
/// data to the process that owns the channel.
///
/// See the `send` function for sending of values using a sender, and
/// `new_channel` for creation of a sender.
///
pub opaque type Sender(msg) {
  Sender(pid: Pid, prepare: Option(fn(msg) -> Dynamic))
}

/// A receiver is one end of a channel, it allows the owning process to receive
/// data sent over the channel via the corresponding sender.
///
/// See the `receive` for receiving values, the `map_receiver` and
/// `merge_receiver` functions for combining receivers, and `new_channel` for
/// creation of a receiver.
///
pub external type Receiver(msg)

external fn new_receiver(reference) -> Receiver(msg) =
  "gleam_otp_external" "new_receiver"

/// Create a receiver for any system messages sent to the current process.
///
/// If you are using a higher level abstraction such as `gleam/actor` system
/// messages will be handled automatically for you and this function should not
/// be used. If you are using long lived processes without using a higher level
/// abstraction you will need to handle system messages manually.
///
pub external fn system_receiver() -> Receiver(SystemMessage) =
  "gleam_otp_external" "system_receiver"

/// Create a new channel for processes to communicate over, returning a sender
/// and a receiver.
///
/// The Receiver is owned by the process that calls this function and must not
/// be sent to another process. Any process that attempts to receive on a
/// receiver that does not belong to them will crash.
///
pub fn new_channel() -> tuple(Sender(msg), Receiver(msg)) {
  let reference = new_reference()
  let sender =
    self()
    |> new_bare_sender
    |> map_sender(fn(msg) { tuple(reference, msg) })
  let receiver = new_receiver(reference)
  tuple(sender, receiver)
}

// TODO: test
/// Create a new channel sender that sends bare messages direct to the given
/// process.
///
/// The messages sent using this sender are not received on any particular
/// channel and as such are not type checked. Always favour using the
/// `new_channel` function when possible as this will provide a safer and more
/// convenient
///
/// This function may be useful when working with processes written in other BEAM
/// languages as they may not use Gleam's channels to receive messages.
///
pub fn new_bare_sender(pid: Pid) -> Sender(msg) {
  let prepare = fn(msg) { dynamic.from(msg) }
  Sender(pid: pid, prepare: Some(prepare))
}

// TODO: test
/// Close a channel, causing any future messages sent on it to be discarded.
///
/// If the sender is used to send a message after the channel is closed it is
/// still delivered to the receiver process but it will be discarded next time
/// the process runs any receiver.
///
/// If the receiver is for a monitor the monitor is removed and any associated
/// messages are flushed from the message inbox.
///
pub external fn close_channels(Receiver(msg)) -> Nil =
  "gleam_otp_external" "close_channels"

/// Get the pid of the receiver process for a sender.
///
pub fn pid(sender: Sender(msg)) -> Pid {
  sender.pid
}

/// Send a message over a channel.
///
/// This function always succeeds, even if the receiving process has shut down
/// or has closed the channel.
///
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
  Sender(pid: pid, prepare: None)
}

type ProcessMonitorFlag {
  Process
}

external fn erlang_monitor_process(ProcessMonitorFlag, Pid) -> Reference =
  "erlang" "monitor"

// TODO: test
// TODO: test closing
/// Start monitoring a process. When the process exits a message is sent over
/// the returned channel with information about the exit.
///
/// The message is only sent once, when the target process exits. If the
/// process was not alive when this function is called the message will never
/// be received.
///
/// Closing the channel with `close_channels` demonitors the process and
/// flushes any monitor message for this channel from the message inbox.
///
pub fn monitor_process(pid: Pid) -> Receiver(ProcessDown) {
  let reference = erlang_monitor_process(Process, pid)
  new_receiver(tuple(Process, reference))
}

type PortMonitorFlag {
  Port
}

external fn erlang_monitor_port(PortMonitorFlag, Port) -> Reference =
  "erlang" "monitor"

// TODO: test
/// Start monitoring a port. When the port exits a message is sent over
/// the returned channel with information about the exit.
///
/// The message is only sent once, when the target port exits. If the port was
/// not alive when this function is called the message will never be received.
///
/// Closing the channel with `close_channels` demonitors the port and flushes
/// any monitor message for this channel from the message inbox.
///
pub fn monitor_port(port: Port) -> Receiver(PortDown) {
  let reference = erlang_monitor_port(Port, port)
  new_receiver(tuple(Port, reference))
}

/// A message received when a monitored process exits.
///
pub type ProcessDown {
  ProcessDown(pid: Pid, reason: Dynamic)
}

/// A message received when a monitored port exits.
///
pub type PortDown {
  PortDown(port: Port, reason: Dynamic)
}

/// Receive a message from one of the channels in a receiver.
///
/// Be careful! This function does not return until there is a message to
/// receive. If no message is received then the process will be stuck waiting
/// forever.
///
pub external fn receive_forever(Receiver(msg)) -> msg =
  "gleam_otp_external" "run_receiver_forever"

/// Discard all messages on the channels in a given receiver.
///
/// This function must be used carefully, it may cause caller processes to
/// crash when they do not receive a reply as their request has been dropped.
///
pub external fn flush(Receiver(msg)) -> Int =
  "gleam_otp_external" "flush_receiver"

// TODO: test
/// Merge one receiver into another, producing a receiver that contains the
/// channels of both.
///
/// If a channel is found in both receivers any mapping function from the
/// second receiver is used in the new receiver.
///
pub external fn merge_receiver(Receiver(a), Receiver(a)) -> Receiver(a) =
  "gleam_otp_external" "merge_receiver"

/// A message received when a linked process exits when the current process is
/// trapping exits.
///
pub type Exit {
  Exit(pid: Pid, reason: Dynamic)
}

// TODO: test
/// Receive a message that does not belong to any particular channel.
///
/// This function is typically not very useful when working with Gleam but it
/// useful when working with Erlang code that sends messages to your code.
///
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

// TODO: test
/// Send an untrappable `kill` exit signal to the target process.
///
/// See the documentation for the Erlang [`erlang:exit`][1] function for more
/// information.
///
/// [1]: https://erlang.org/doc/man/erlang.html#exit-1
///
pub fn kill(pid: Pid) -> Nil {
  erlang_kill(pid, Kill)
  Nil
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

/// Start a new process from a given function.
///
/// The new process is linked to the parent process so if it crashes the parent
/// will also crash. If you wish your program to tolerate crashes see the
/// supervisor module.
///
pub external fn start(fn() -> anything) -> Pid =
  "erlang" "spawn_link"

/// Start a new process from a given function.
///
/// The new process is not linked to the parent process so if it crashes the
/// issue may be silently ignored, leaving your program in an invalid state.
///
pub external fn start_unlinked(fn() -> anything) -> Pid =
  "erlang" "spawn"

/// Receive a message from one of the channels in a given receiver, removing it
/// from the process inbox and returning it.
///
/// If there are no messages for this receiver in the inbox and one is not
/// received within the timeout then an error is returned.
///
pub external fn receive(
  receiver: Receiver(msg),
  timeout: Int,
) -> Result(msg, Nil) =
  "gleam_otp_external" "run_receiver"

/// An error returned when making a call to a process.
///
pub type CallError(msg) {
  /// The process being called exited before it sent a response.
  ///
  CalleeDown(reason: Dynamic)

  /// The process being called did not response within the permitted amount of
  /// time.
  ///
  CallTimeout
}

/// Add a transformation function to a receiver. When a message is received
/// using this receiver the tranformation function is applied to the message.
///
/// This function can be used to change the type of messages received and may
/// be useful when combined with the `merge_receiver` function.
///
pub external fn map_receiver(Receiver(a), with: fn(a) -> b) -> Receiver(b) =
  "gleam_otp_external" "map_receiver"

/// Add a transformation function to a sender. When a message is sent using this
/// sender the tranformation function is applied to the message before it is
/// sent.
///
/// This function can be used to change the type of messages sent and may
/// be useful to change the type of a sender before giving it to another process.
///
/// You may notice that this function takes a mapper function from `b` to `a`
/// rather than from `a` to `b` as you would find in functions like `list.map`
/// and `receiver.map`. This style of a map function may be called a
/// _"contravarient"_ map.
///
pub fn map_sender(sender: Sender(a), with mapper: fn(b) -> a) -> Sender(b) {
  let wrap = function.compose(mapper, _)
  let prepare = option.map(sender.prepare, wrap)
  Sender(prepare: prepare, pid: sender.pid)
}

// TODO: test error paths
// This function is based off of Erlang's gen:do_call/4.
/// Send a message over a channel and wait for a reply.
///
/// If the receiving process exits or does not reply within the allowed amount
/// of time then an error is returned.
///
pub fn try_call(
  sender: Sender(request),
  make_request: fn(Sender(response)) -> request,
  timeout: Int,
) -> Result(response, CallError(response)) {
  let tuple(reply_sender, reply_receiver) = new_channel()

  // Monitor the callee process so we can tell if it goes down (meaning we
  // won't get a reply)
  let monitor = monitor_process(pid(sender))

  // Send the request to the process over the channel
  send(sender, make_request(reply_sender))

  let receiver =
    reply_receiver
    |> map_receiver(Ok)
    |> merge_receiver(map_receiver(
      monitor,
      fn(down: ProcessDown) { Error(CalleeDown(reason: down.reason)) },
    ))

  // Await a reply or handle failure modes (timeout, process down, etc)
  let res = receive(receiver, timeout)

  // Demonitor the process and close the channels as we're done
  close_channels(receiver)

  // Prepare an appropriate error (if present) for the caller
  case res {
    Error(Nil) -> Error(CallTimeout)
    Ok(res) -> res
  }
}

// TODO: test error paths
/// Send a message over a channel and wait for a reply.
///
/// If the receiving process exits or does not reply within the allowed amount
/// of time the calling process crashes. If you wish an error to be returned
/// instead see the `try_call` function.
///
pub fn call(
  sender: Sender(request),
  make_request: fn(Sender(response)) -> request,
  timeout: Int,
) -> response {
  assert Ok(resp) = try_call(sender, make_request, timeout)
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

/// Get the number of messages in the calling processes' inbox message queue.
///
pub fn message_queue_size(pid: Pid) -> Int {
  process_info_message_queue_length(pid, MessageQueueLen).1
}

/// Start trapping exits within the current process and return a receiver.
///
/// When not trapping exits if a linked process crashes an `Exit` message is
/// sent over the channel. This is the normal behaviour before this function is
/// called.
///
/// When trapping exits (after this function is called) if a linked process
/// crashes an `Exit` message is sent over the channel.
///
pub external fn trap_exits() -> Receiver(Exit) =
  "gleam_otp_external" "trap_exits"

/// Stop trapping exits, causing any crashes in linked processes to also crash
/// this process.
///
/// See also the `trap_exits` function.
///
pub external fn stop_trapping_exits() -> Nil =
  "gleam_otp_external" "stop_trapping_exits"

pub external type Timer

external fn erlang_send_after(Int, Pid, msg) -> Timer =
  "erlang" "send_after"

external fn fake_timer() -> Timer =
  "erlang" "make_ref"

/// Send a message over a channel after a specified timeout.
///
pub fn send_after(sender: Sender(msg), delay: Int, message: msg) -> Timer {
  case sender.prepare {
    Some(prepare) -> erlang_send_after(delay, sender.pid, prepare(message))
    None -> fake_timer()
  }
}

external fn erlang_cancel_timer(Timer) -> Dynamic =
  "erlang" "cancel_timer"

/// Values returned when a timer is cancelled.
///
pub type Cancelled {
  /// The timer could not be found. It probably has already triggered.
  ///
  TimerNotFound

  /// The timer was found and cancelled before it triggered.
  ///
  Cancelled(time_remaining: Int)
}

/// Cancel a given timer, causing it not to trigger if it has not done already.
///
pub fn cancel_timer(timer: Timer) -> Cancelled {
  case dynamic.int(erlang_cancel_timer(timer)) {
    Ok(i) -> Cancelled(i)
    Error(_) -> TimerNotFound
  }
}

external fn erlang_link(Pid) -> Bool =
  "erlang" "link"

// TODO: test
/// Creates a link between the calling process and another process.
///
/// When a process crashes any linked processes will also crash. This is useful
/// to ensure that groups of processes that depend on each other all either
/// succeed or fail together.
///
/// See the `gleam/otp/supervisor` module and the `trap_exits` function for
/// mechanisms for handling process crashes.
///
pub fn link(pid: Pid) -> Nil {
  erlang_link(pid)
  Nil
}

external fn erlang_unlink(pid: Pid) -> Bool =
  "erlang" "link"

// TODO: test
/// Removes any existing link between the caller process and the target process.
///
pub fn unlink(pid: Pid) -> Nil {
  erlang_unlink(pid)
  Nil
}
