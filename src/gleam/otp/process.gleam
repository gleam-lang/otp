// TODO: README
// TODO: monitor

import gleam/atom
import gleam/dynamic.{Dynamic}

// A Pid (or Process identifier) is a reference to an OTP process, which is a
// lightweight thread that communicates by sending and receiving messages.
//
// The Pid type is parameterised with the message type that the Pid can
// receive.
//
pub external type Pid(accepted_message);

// UnknownMessage is a type that has no values, it can never be constructed!
//
// This is useful because we can safely cast a Pid of dynamic message type to a Pid
// of with message type of UnknownMessage as there's no risk of a
// UnknownMessage value being sent to the process.
//
pub type UnknownMessage {
  ImpossibleRecursiveConstructor(UnknownMessage)
}

// Cast a Pid with a known message type to one with an unknown message type,
// erasing type information and making it impossible to send messages to.
//
// This may be useful for when you wish to have a list or other data structure
// of Pids but don't care about what messages they can receieve.
//
pub external fn make_opaque(Pid(msg)) -> Pid(UnknownMessage)
  = "gleam_otp_process_external" "cast";

// Unsafely cast a Pid of an unknown message type to one with a known type.
// This function should be avoided where possible as when using it Gleam is
// unable to verify that the process can handle a message being sent to it.
//
// If you use this and there is a mistake the process will likely crash when
// you send a message to it!
//
pub external fn unsafe_downcast(Pid(UnknownMessage)) -> Pid(known_message)
  = "gleam_otp_process_external" "cast";

pub type LinkResult {
  Linked
  ProcessNotFound
}

// Creates a link between the calling process and the process for a given Pid.
//
// See the [Erlang documentation][erl] for more information.
// [erl]: http://erlang.org/doc/man/erlang.html#link-1
//
pub external fn link(Pid(msg)) -> LinkResult
  = "gleam_otp_process_external" "link";

// Removes a link between the calling process and the process for a given Pid.
// If there is an EXIT message in the calling process mailbox for the removed
// link they are also removed.
//
// See the [Erlang documentation][erl] for more information.
// [erl]: http://erlang.org/doc/man/erlang.html#unlink-1
//
pub external fn unlink(Pid(msg)) -> Nil
  = "gleam_otp_process_external" "unlink";

// Send a message to a process.
//
// Message sending is asynchronous and this function will likely return before
// the message is handled by the receiving processes.
//
// See the [Erlang documentation][erl] for more information.
// [erl]: http://erlang.org/doc/man/erlang.html#send-2
//
pub external fn send(to: Pid(msg), msg: msg) -> msg
  = "erlang" "send";

// Check to see whether the process for a given Pid is alive.
//
// See the [Erlang documentation][erl] for more information.
// [erl]: http://erlang.org/doc/man/erlang.html#is_process_alive-1
//
pub external fn is_alive(Pid(msg)) -> Bool
  = "erlang" "is_process_alive";

// Sends an exit signal to a process, indicating that that process is to shut
// down.
//
// See the [Erlang documentation][erl] for more information.
// [erl]: http://erlang.org/doc/man/erlang.html#exit-2
//
pub external fn send_exit(to: Pid(msg), because: reason) -> Nil
  = "gleam_otp_process_external" "send_exit";

// A reference to the current process, parameterised with the type of message
// that the process will accept. This value is given to a process at start and
// is used to enable inference of the messages that a process can safely
// receive.
//
// Warning! Never send this value to another process!
//
// It is possible to send this value in a message so the type checker says it
// is ok for the receiever process to accept messages of the original process'
// message type, however this should never be done. If this is done the type
// checker will be mislead and will not be able to check your code correctly,
// making errors and runtime crashes likely.
//
pub external type Self(accepted_message)

// Get the Pid of the current process.
//
pub external fn own_pid(Self(msg)) -> Pid(msg)
  = "gleam_otp_process_external" "own_pid";

// Get the Pid of the current process, without requiring Self.
//
// Because the the Self value is not used here the compiler cannot tell the
// type of the messages that the current process accepts, so it uses the
// UnknownMessage type which cannot be constructed or sent.
//
pub external fn opaque_own_pid() -> Pid(UnknownMessage)
  = "gleam_otp_process_external" "own_pid";

// A private type to ensure we do not leak values from unreliable Erlang
// functions.
external type DoNotLeak

external fn process_dictionary_set(dynamicthing, dynamicthing_else) -> DoNotLeak
  = "erlang" "put";

external fn process_dictionary_delete(dynamicthing) -> DoNotLeak
  = "erlang" "erase";

// The key used to store in the process dictionary the the trap_exit msg
// constructor.
type GleamOtpProcessExitMsgConstructor {
  GleamOtpProcessExitMsgConstructor
}

// TODO: document
pub fn trap_exit(_: Self(msg), constructor: fn(Pid(UnknownMessage), Dynamic) -> msg) {
  process_dictionary_set(GleamOtpProcessExitMsgConstructor, constructor)
  Nil
}

// TODO: document
// TODO: test
pub fn no_trap_exit() {
  process_dictionary_delete(GleamOtpProcessExitMsgConstructor)
  Nil
}

type FlagKey {
  MaxHeapSize
  MessageQueueData
  MinBinVheapSize
  MinHeapSize
  Priority
  SaveCalls
  Sensitive
}

external fn apply_process_flag(FlagKey, dynamicthing) -> DoNotLeak
  = "erlang" "process_flag";

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_min_heap_size(in_words limit: Int) {
  apply_process_flag(MinHeapSize, limit)
  Nil
}

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_min_bin_vheap_size(in_words limit: Int) {
  apply_process_flag(MinBinVheapSize, limit)
  Nil
}

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_max_heap_size(in_words limit: Int) {
  apply_process_flag(MaxHeapSize, limit)
  Nil
}

pub type DataLocation {
  OnHeap
  OffHeap
}

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_message_queue_data(location: DataLocation) {
  apply_process_flag(MessageQueueData, location)
  Nil
}

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_save_calls(limit: Int) {
  apply_process_flag(SaveCalls, limit)
  Nil
}

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_sensitive(is_sensitive: Int) {
  apply_process_flag(Sensitive, is_sensitive)
  Nil
}

pub type SchedulerPriority {
  Low
  Normal
  High
  Max
}

// http://erlang.org/doc/man/erlang.html#process_flag-2
// TODO: document
// TODO: test
pub fn set_priority(level: SchedulerPriority) {
  apply_process_flag(Priority, level)
  Nil
}

// TODO: document
pub external fn spawn(fn(Self(msg)) -> dynamicthing) -> Pid(msg)
  = "gleam_otp_process_external" "do_spawn";

// TODO: document
pub external fn spawn_link(fn(Self(msg)) -> dynamicthing) -> Pid(msg)
  = "gleam_otp_process_external" "do_spawn_link";

// TODO: document
pub external fn receive(Self(msg), waiting_max: Int) -> Result(msg, Nil)
  = "gleam_otp_process_external" "do_receive";

// TODO: document
pub external fn opaque_receive(waiting_max: Int) -> Result(Dynamic, Nil)
  = "gleam_otp_process_external" "do_receive";
