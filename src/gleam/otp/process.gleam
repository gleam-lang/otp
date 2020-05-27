// TODO: README
// TODO: monitor
// TODO: link
//
import gleam/atom
import gleam/result
import gleam/dynamic.{Dynamic}
import gleam/option.{Option, None}

external type DoNotLeak

/// A Pid (or Process identifier) is a reference to an OTP process, which is a
/// lightweight thread that communicates by sending and receiving messages.
///
/// The Pid type is parameterised with the message type that the Pid can
/// receive.
///
pub external type Pid(accepted_message)

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
// Special thanks to Peter Saxton for designing the synchronous message system
pub external type From(reply)

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
  // Suspend
  // Resume
  // GetStatus
  GetState(From(Dynamic))
}

// TODO: document
pub external type SystemResponse

pub type StartResult(message) =
  Result(Pid(message), ExitReason)

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

/// UnknownMessage is a type that has no values, it can never be constructed!
///
/// This is useful because we can safely coerce a Pid of dynamic message type to a Pid
/// of with message type of UnknownMessage as there's no risk of a
/// UnknownMessage value being sent to the process.
///
pub external type UnknownMessage

/// Coerce a Pid with a known message type to one with an unknown message type,
/// erasing type information and making it impossible to send messages to.
///
/// This may be useful for when you wish to have a list or other data structure
/// of Pids but don't care about what messages they can receieve.
///
pub external fn make_opaque(Pid(msg)) -> Pid(UnknownMessage) =
  "gleam_otp_process_external" "unsafe_coerce"

/// Unsafely coerce a Pid of an unknown message type to one with a known type.
/// This function should be avoided where possible as when using it Gleam is
/// unable to verify that the process can handle a message being sent to it.
///
/// If you use this and there is a mistake the process will likely crash when
/// you send a message to it!
///
pub external fn unsafe_downcast(Pid(UnknownMessage)) -> Pid(known_message) =
  "gleam_otp_process_external" "unsafe_coerce"

pub external fn erl_async_send(to: Pid(msg), msg: msg) -> msg =
  "erlang" "send"

/// Send a message to a process.
///
/// Message sending is asynchronous and this function will likely return before
/// the message is handled by the receiving processes.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#send-2
///
pub fn async_send(to receiever: Pid(msg), msg msg: msg) -> Nil {
  erl_async_send(receiever, msg)
  Nil
}

// TODO: document
pub external fn sync_send(
  to: Pid(msg),
  message: fn(From(reply)) -> msg,
  timeout: Int,
) -> reply =
  "gleam_otp_process_external" "sync_send"

/// Check to see whether the process for a given Pid is alive.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
///
pub external fn is_alive(Pid(msg)) -> Bool =
  "erlang" "is_process_alive"

// TODO: test
/// Sends an exit signal to a process, indicating that that process is to shut
/// down.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
pub external fn send_exit(to: Pid(msg), because: reason) -> Nil =
  "gleam_otp_process_external" "send_exit"

pub external type DebugOptions

/// A reference to the current process, parameterised with the type of message
/// that the process will accept. This value is given to a process at start and
/// is used to enable inference of the messages that a process can safely
/// receive.
///
/// Warning! Never send this value to another process!
///
/// It is possible to send this value in a message so the type checker says it
/// is ok for the receiever process to accept messages of the original process'
/// message type, however this should never be done. If this is done the type
/// checker will be mislead and will not be able to check your code correctly,
/// making errors and runtime crashes likely.
///
pub type Self(msg) {
  Self(parent: Pid(UnknownMessage), pid: Pid(msg), debug: DebugOptions)
}

/// Get the Pid of the current process.
///
pub external fn own_pid(Self(msg)) -> Pid(msg) =
  "gleam_otp_process_external" "own_pid"

/// Get the Pid of the current process, without requiring Self.
///
/// Because the the Self value is not used here the compiler cannot tell the
/// type of the messages that the current process accepts, so it uses the
/// UnknownMessage type which cannot be constructed or sent.
///
pub external fn opaque_own_pid() -> Pid(UnknownMessage) =
  "gleam_otp_process_external" "own_pid"

// TODO: document
pub external fn started(self: Self(msg)) -> Nil =
  "gleam_otp_process_external" "started"

// TODO: document
pub external fn failed_to_start(self: Self(msg), ExitReason) -> Nil =
  "gleam_otp_process_external" "failed_to_start"

// TODO: document
pub type Spec(msg) {
  Spec(
    routine: fn(Self(msg)) -> ExitReason,
    exit_trapper: Option(fn(Pid(UnknownMessage), ExitReason) -> msg),
  )
}

// TODO: test
// TODO: document
// TODO: ensure to document that `started` must be called
pub external fn start_spec(Spec(msg)) -> StartResult(msg) =
  "gleam_otp_process_external" "start"

// TODO: test
// TODO: document
// TODO: ensure to document that `started` must be called
pub fn start(routine: fn(Self(msg)) -> ExitReason) -> StartResult(msg) {
  let spec = Spec(routine: routine, exit_trapper: None)
  start_spec(spec)
}

external fn debug(a) -> a =
  "erlang" "display"

// TODO: document
pub external fn receive(Self(msg), timeout: Int) -> Result(Message(msg), Nil) =
  "gleam_otp_process_external" "receive_any"

// TODO: document
pub external fn receive_forever(Self(msg)) -> Message(msg) =
  "gleam_otp_process_external" "receive_any_forever"

// TODO: document
// TODO: test
pub external fn reply(to: From(reply), with: reply) -> reply =
  "gen" "reply"
