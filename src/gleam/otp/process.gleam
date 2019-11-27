// TODO: README
// TODO: Linking
// TODO: Trapping exits
// TODO: spawn
// TODO: spawn_link

import gleam/atom

/// A Pid (or Process identifier) is a reference to an OTP process, which is a
/// lightweight thread that communicates by sending and receiving messages.
///
/// The Pid type is parameterised with the message type that the Pid can
/// receive.
///
pub external type Pid(accepted_message);

/// UnknownMessage is a type that has no values, it can never be constructed!
///
/// This is useful because we can safely cast a Pid of any message type to a Pid
/// of with message type of UnknownMessage as there's no risk of a
/// UnknownMessage value being sent to the process.
///
pub enum UnknownMessage {
  ImpossibleRecursiveConstructor(UnknownMessage)
}

/// Cast a Pid with a known message type to one with an unknown message type,
/// erasing type information and making it impossible to send messages to.
///
/// This may be useful for when you wish to have a list or other data structure
/// of Pids but don't care about what messages they can receieve.
///
// TODO: test
pub external fn make_opaque(Pid(msg)) -> Pid(UnknownMessage)
  = "gleam_otp_process_external" "cast";

/// Unsafely cast a Pid of an unknown message type to one with a known type.
/// This function should be avoided where possible as when using it Gleam is
/// unable to verify that the process can handle a message being sent to it.
///
// TODO: test
pub external fn unsafe_downcast(Pid(UnknownMessage)) -> Pid(known_message)
  = "gleam_otp_process_external" "cast";

pub enum LinkResult {
  Linked
  ProcessNotFound
}

/// Creates a link between the calling process and the process for a given Pid.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#link-1
///
/// TODO: test
pub external fn link(Pid(msg)) -> LinkResult
  = "gleam_otp_process_external" "link";

/// Removes a link between the calling process and the process for a given Pid.
/// If there is an EXIT message in the calling process mailbox for the removed
/// link they are also removed.
///
// TODO: test
pub external fn unlink(Pid(msg)) -> Nil
  = "gleam_otp_process_external" "unlink";

/// Send a message to a process.
///
/// Message sending is asynchronous and this function will likely return before
/// the message is handled by the receiving processes.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#send-2
///
// TODO: test
pub external fn send(to: Pid(msg), msg: msg) -> msg
  = "erlang" "send";

/// Check to see whether the process for a given Pid is alive.
///
// TODO: test
pub external fn is_alive(Pid(msg)) -> Bool
  = "erlang" "is_process_alive";

/// Sends an exit signal to a process, indicating that that process is to shut
/// down.
///
/// See the [Erlang documentation][erl] for more information.
/// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
///
// TODO: test
pub external fn send_exit(to: Pid(msg), because: reason) -> Nil
  = "gleam_otp_process_external" "send_exit";
