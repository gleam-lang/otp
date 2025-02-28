//// This module provides the _Actor_ abstraction, one of the most common
//// building blocks of Gleam OTP programs.
//// 
//// An Actor is a process like any other BEAM process and can be used to hold
//// state, execute code, and communicate with other processes by sending and
//// receiving messages. The advantage of using the actor abstraction over a bare
//// process is that it provides a single interface for commonly needed
//// functionality, including support for the [tracing and debugging
//// features in OTP][erlang-sys].
////
//// Gleam's Actor is similar to Erlang's `gen_server` and Elixir's `GenServer`
//// but differs in that it offers a fully typed interface. This different API is
//// why Gleam uses the name Actor rather than some variation of generic-server.
////
//// [erlang-sys]: https://www.erlang.org/doc/man/sys.html
////
//// ## Example
////
//// An Actor can be used to create a client-server interaction between an Actor
//// (the server) and other processes (the clients). In this example we have an
//// Actor that works as a stack, allowing clients to push and pop elements.
////
//// ```gleam
//// pub fn main() {
////   // Start the actor with initial state of an empty list, and the
////   // `handle_message` callback function (defined below).
////   // We assert that it starts successfully.
////   // 
////   // In real-world Gleam OTP programs we would likely write wrapper functions
////   // called `start`, `push` `pop`, `shutdown` to start and interact with the
////   // Actor. We are not doing that here for the sake of showing how the Actor 
////   // API works.
////   let assert Ok(my_actor) = actor.start([], handle_message)
//// 
////   // We can send a message to the actor to push elements onto the stack.
////   process.send(my_actor, Push("Joe"))
////   process.send(my_actor, Push("Mike"))
////   process.send(my_actor, Push("Robert"))
//// 
////   // The `Push` message expects no response, these messages are sent purely for
////   // the side effect of mutating the state held by the actor.
////   //
////   // We can also send the `Pop` message to take a value off of the actor's
////   // stack. This message expects a response, so we use `process.call` to send a
////   // message and wait until a reply is received.
////   //
////   // In this instance we are giving the actor 10 milliseconds to reply, if the
////   // `call` function doesn't get a reply within this time it will panic and
////   // crash the client process.
////   let assert Ok("Robert") = process.call(my_actor, Pop, 10)
////   let assert Ok("Mike") = process.call(my_actor, Pop, 10)
////   let assert Ok("Joe") = process.call(my_actor, Pop, 10)
//// 
////   // The stack is now empty, so if we pop again the actor replies with an error.
////   let assert Error(Nil) = process.call(my_actor, Pop, 10)
//// 
////   // Lastly, we can send a message to the actor asking it to shut down.
////   process.send(my_actor, Shutdown)
//// }
//// ```
////
//// Here is the code that is used to implement this actor:
////
//// ```gleam
//// import gleam/erlang/process.{type Subject}
//// import gleam/otp/actor
////
//// // First step of implementing the stack Actor is to define the message type that
//// // it can receive.
//// //
//// // The type of the elements in the stack is not fixed so a type parameter is used
//// // for it instead of a concrete type such as `String` or `Int`.
//// pub type Message(element) {
////   // The `Shutdown` message is used to tell the actor to stop.
////   // It is the simplest message type, it contains no data.
////   Shutdown
//// 
////   // The `Push` message is used to add a new element to the stack.
////   // It contains the item to add, the type of which is the `element`
////   // parameterised type.
////   Push(push: element)
//// 
////   // The `Pop` message is used to remove an element from the stack.
////   // It contains a `Subject`, which is used to send the response back to the
////   // message sender. In this case the reply is of type `Result(element, Nil)`.
////   Pop(reply_with: Subject(Result(element, Nil)))
//// }
//// 
//// // The last part is to implement the `handle_message` callback function.
//// //
//// // This function is called by the Actor for each message it receives.
//// // Actor is single threaded and only does one thing at a time, so it handles
//// // messages sequentially and one at a time, in the order they are received.
//// //
//// // The function takes the message and the current state, and returns a data
//// // structure that indicates what to do next, along with the new state.
//// fn handle_message(
////  message: Message(e),
////  stack: List(e),
//// ) -> actor.Next(Message(e), List(e)) {
////   case message {
////     // For the `Shutdown` message we return the `actor.Stop` value, which causes
////     // the actor to discard any remaining messages and stop.
////     Shutdown -> actor.Stop(process.Normal)
//// 
////     // For the `Push` message we add the new element to the stack and return
////     // `actor.continue` with this new stack, causing the actor to process any
////     // queued messages or wait for more.
////     Push(value) -> {
////       let new_state = [value, ..stack]
////       actor.continue(new_state)
////     }
//// 
////     // For the `Pop` message we attempt to remove an element from the stack,
////     // sending it or an error back to the caller, before continuing.
////     Pop(client) ->
////       case stack {
////         [] -> {
////           // When the stack is empty we can't pop an element, so we send an
////           // error back.
////           process.send(client, Error(Nil))
////           actor.continue([])
////         }
//// 
////         [first, ..rest] -> {
////           // Otherwise we send the first element back and use the remaining
////           // elements as the new state.
////           process.send(client, Ok(first))
////           actor.continue(rest)
////         }
////       }
////   }
//// }
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{
  type ExitReason, type Pid, type Selector, type Subject, Abnormal, Killed,
}
import gleam/function
import gleam/option.{type Option, None, Some}
import gleam/otp/system.{
  type DebugState, type Mode, type StatusInfo, type SystemMessage, GetState,
  GetStatus, Resume, Running, StatusInfo, Suspend, Suspended,
}
import gleam/string

type Message(message) {
  /// A regular message excepted by the process
  Message(message)

  /// An OTP system message, for debugging or maintenance
  System(SystemMessage)

  /// An unexpected message
  Unexpected(Dynamic)
}

// TODO: make opaque
// TODO: changelog argument order change
/// The type used to indicate what to do after handling a message.
///
pub type Next(state, message) {
  /// Continue handling messages.
  ///
  /// An optional selector can be provided to changes the messages that the
  /// actor is handling. This replaces any selector that was previously given
  /// in the actor's `init` callback, or in any previous `Next` value.
  ///
  Continue(state: state, selector: Option(Selector(message)))

  /// Stop handling messages and shut down.
  ///
  Stop(ExitReason)
}

pub fn continue(state: state) -> Next(state, message) {
  Continue(state:, selector: None)
}

/// Provide a selector to change the messages that the actor is handling
/// going forward. This replaces any selector that was previously given
/// in the actor's `init` callback, or in any previous `Next` value.
///
pub fn with_selector(
  value: Next(state, message),
  selector: Selector(message),
) -> Next(state, message) {
  case value {
    Continue(state, _) -> Continue(state, Some(selector))
    Stop(_) -> value
  }
}

type Self(state, msg) {
  Self(
    /// The mode the actor is currently in, either active or suspended.
    mode: Mode,
    /// The pid of the process that started this actor.
    parent: Pid,
    /// The state of this actor, provided by the programmer.
    state: state,
    /// The selector that actor is currently using to reveive messages. This
    /// can be changed by the `Next` value returned by the actor's `loop` callback.
    selector: Selector(Message(msg)),
    /// An opaque value used by the OTP system debug APIs.
    debug_state: DebugState,
    /// The message handling code provided by the programmer.
    message_handler: fn(state, msg) -> Next(state, msg),
  )
}

// TODO: document
// TODO: test
pub type Started(data) {
  Started(pid: Pid, returned: data)
}

// TODO: document
// TODO: test
pub opaque type Initialised(state, message, return) {
  Initialised(state: state, selector: Selector(message), return: return)
}

// TODO: document
// TODO: test
pub fn initialised(state: state) -> Initialised(state, message, Nil) {
  Initialised(state, process.new_selector(), Nil)
}

// TODO: document
// TODO: test
pub fn selecting(
  initialised: Initialised(state, old_message, return),
  selector: Selector(message),
) -> Initialised(state, message, return) {
  Initialised(..initialised, selector:)
}

// TODO: document
// TODO: test
pub fn returning(
  initialised: Initialised(state, message, old_return),
  return: return,
) -> Initialised(state, message, return) {
  Initialised(..initialised, return:)
}

// TODO: redesign
// TODO: opaque
// TODO: builder API
// TODO: document
pub opaque type Builder(state, message, return) {
  Builder(
    /// The initialisation functionality for the actor. This function is called
    /// just after the actor starts but before the channel sender is returned
    /// to the parent.
    ///
    /// This function is used to ensure that any required data or state is
    /// correct. If this function returns an error it means that the actor has
    /// failed to start and an error is returned to the parent.
    ///
    initialise: fn() -> Result(Initialised(state, message, return), String),
    /// How many milliseconds the `init` function has to return before it is
    /// considered to have taken too long and failed.
    ///
    initialisation_timeout: Int,
    // TODO: changelog argument order change
    /// This function is called to handle each message that the actor receives.
    ///
    on_message: fn(state, message) -> Next(state, message),
  )
}

// TODO: document
// TODO: test
pub fn new(state: state) -> Builder(state, message, Subject(message)) {
  Builder(
    initialise: fn() {
      let subject = process.new_subject()
      let selector =
        process.new_selector() |> process.selecting(subject, function.identity)
      initialised(state)
      |> selecting(selector)
      |> returning(subject)
      |> Ok
    },
    initialisation_timeout: 1000,
    on_message: fn(_, _) { Stop(process.Normal) },
  )
}

pub fn new_with_initialiser(
  timeout: Int,
  initialise: fn() -> Result(Initialised(state, message, return), String),
) -> Builder(state, message, return) {
  Builder(initialise:, initialisation_timeout: timeout, on_message: fn(_, _) {
    Stop(process.Normal)
  })
}

pub fn on_message(
  builder: Builder(state, message, return),
  handler: fn(state, message) -> Next(state, message),
) -> Builder(state, message, return) {
  Builder(..builder, on_message: handler)
}

// TODO: Check needed functionality here to be OTP compatible
fn exit_process(reason: ExitReason) -> ExitReason {
  case reason {
    Abnormal(reason) -> process.send_abnormal_exit(process.self(), reason)
    Killed -> process.kill(process.self())
    _ -> Nil
  }

  reason
}

fn receive_message(self: Self(state, msg)) -> Message(msg) {
  let selector = case self.mode {
    // When suspended we only respond to system messages
    Suspended ->
      process.new_selector()
      |> selecting_system_messages

    // When running we respond to all messages
    Running ->
      // The actor needs to handle various different messages:
      //
      // - OTP system messages. These are handled by the actor for the
      //   programmer, they don't need to do anything.
      // - Messages sent to the subject the actor creates during initialisation
      //   and returns to the parent.
      // - Any arbitrary messages the programmer expects the actor to receive.
      //   For example, messages sent by a pubsub system where it does not
      //   support using the actor's subject.
      // - Any unexpected messages.
      //
      // We add the handler for unexpected messages first so that the user
      // supplied selector can override it if desired.
      process.new_selector()
      |> process.selecting_anything(Unexpected)
      |> process.merge_selector(self.selector)
      |> selecting_system_messages
  }

  process.select_forever(selector)
}

fn selecting_system_messages(
  selector: Selector(Message(msg)),
) -> Selector(Message(msg)) {
  selector
  |> process.selecting_record3(atom.create("system"), convert_system_message)
}

@external(erlang, "gleam_otp_external", "convert_system_message")
fn convert_system_message(a: Dynamic, b: Dynamic) -> Message(msg)

fn process_status_info(self: Self(state, msg)) -> StatusInfo {
  StatusInfo(
    module: atom.create("gleam@otp@actor"),
    parent: self.parent,
    mode: self.mode,
    debug_state: self.debug_state,
    state: dynamic.from(self.state),
  )
}

fn loop(self: Self(state, msg)) -> ExitReason {
  case receive_message(self) {
    // An OTP system message. This is handled by the actor for the programmer,
    // behind the scenes.
    System(system) ->
      case system {
        GetState(callback) -> {
          callback(dynamic.from(self.state))
          loop(self)
        }
        Resume(callback) -> {
          callback()
          loop(Self(..self, mode: Running))
        }
        Suspend(callback) -> {
          callback()
          loop(Self(..self, mode: Suspended))
        }
        GetStatus(callback) -> {
          callback(process_status_info(self))
          loop(self)
        }
      }

    // An unexpected message. It this is reached then the programmer has not
    // handled this, so log a warning.
    Unexpected(message) -> {
      log_warning(
        charlist.from_string("Actor discarding unexpected message: ~s"),
        [charlist.from_string(string.inspect(message))],
      )
      loop(self)
    }

    // A regular message that the programmer is expecting, either over the
    // subject or some other messsage that the programmer's selector expects.
    Message(msg) ->
      case self.message_handler(self.state, msg) {
        Stop(reason) -> exit_process(reason)

        Continue(state: state, selector: new_selector) -> {
          let selector = case new_selector {
            None -> self.selector
            Some(s) -> process.map_selector(s, Message)
          }
          loop(Self(..self, state: state, selector: selector))
        }
      }
  }
}

// TODO: replace this when we have Gleam bindings to the logger
@external(erlang, "logger", "warning")
fn log_warning(a: Charlist, b: List(Charlist)) -> Nil

// Run automatically when the actor is first started.
fn initialise_actor(
  builder: Builder(state, msg, return),
  parent: Pid,
  ack: Subject(Result(Started(return), ExitReason)),
) -> ExitReason {
  let pid = process.self()

  // Run the programmer supplied initialisation code.
  let result = builder.initialise()

  case result {
    // Init was OK, send the subject to the parent and start handling messages.
    Ok(Initialised(state:, selector:, return:)) -> {
      let selector = process.map_selector(selector, Message)
      // Signal to parent that the process has initialised successfully
      process.send(ack, Ok(Started(pid:, returned: return)))
      // Start message receive loop
      let self =
        Self(
          state: state,
          parent:,
          selector: selector,
          message_handler: builder.on_message,
          debug_state: system.debug_state([]),
          mode: Running,
        )
      loop(self)
    }

    // The init failed. Send the reason back to the parent, but exit normally.
    Error(reason) -> {
      process.send(ack, Error(Abnormal(reason)))
      exit_process(process.Normal)
    }
  }
}

pub type StartError {
  InitTimeout
  InitFailed(ExitReason)
  InitCrashed(Dynamic)
}

/// The result of starting a Gleam actor.
///
/// This type is compatible with Gleam supervisors. If you wish to convert it
/// to a type compatible with Erlang supervisors see the `ErlangStartResult`
/// type and `erlang_start_result` function.
///
pub type StartResult(msg) =
  Result(Subject(msg), StartError)

/// An Erlang supervisor compatible process start result.
///
pub type ErlangStartResult =
  Result(Pid, Dynamic)

type StartInitMessage(data) {
  Ack(Result(Started(data), ExitReason))
  Mon(process.ProcessDown)
}

// TODO: test init_timeout. Currently if we test it eunit prints an error from
// the process death. How do we avoid this?
//
/// Start an actor from a given specification. If the actor's `init` function
/// returns an error or does not return within `init_timeout` then an error is
/// returned.
///
/// If you do not need to specify the initialisation behaviour of your actor
/// consider using the `start` function.
///
pub fn start(
  builder: Builder(state, msg, return),
) -> Result(Started(return), StartError) {
  let ack_subject = process.new_subject()
  let self = process.self()

  let child =
    process.spawn(fn() { initialise_actor(builder, self, ack_subject) })

  let monitor = process.monitor(child)
  let selector =
    process.new_selector()
    |> process.selecting(ack_subject, Ack)
    |> process.selecting_process_down(monitor, Mon)

  let result = case process.select(selector, builder.initialisation_timeout) {
    // Child started OK
    Ok(Ack(Ok(subject))) -> Ok(subject)

    // Child initialiser returned an error
    Ok(Ack(Error(reason))) -> Error(InitFailed(reason))

    // Child went down while initialising
    Ok(Mon(down)) -> Error(InitCrashed(down.reason))

    // Child did not finish initialising in time
    Error(Nil) -> {
      // Unlink the child before killing it, so that we only return the error,
      // but don't also send an exit message to the linked parent process.
      process.unlink(child)
      process.kill(child)
      Error(InitTimeout)
    }
  }

  // Remove the monitor used for the starting of the actor as to avoid an extra
  // message arriving at the parent if the child dies later.
  process.demonitor_process(monitor)

  result
}

/// Send a message over a given channel.
///
/// This is a re-export of `process.send`, for the sake of convenience.
///
pub fn send(subject: Subject(msg), msg: msg) -> Nil {
  process.send(subject, msg)
}

/// Send a synchronous message and wait for a response from the receiving
/// process.
///
/// If a reply is not received within the given timeout then the sender process
/// crashes. If you wish to receive a `Result` rather than crashing see the
/// `process.try_call` function.
///
/// This is a re-export of `process.call`, for the sake of convenience.
///
pub fn call(
  subject: Subject(message),
  make_message: fn(Subject(reply)) -> message,
  timeout: Int,
) -> reply {
  process.call(subject, make_message, timeout)
}
