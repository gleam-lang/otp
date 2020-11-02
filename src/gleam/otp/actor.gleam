import gleam/otp/process.{
  Abnormal, DebugState, ExitReason, GetState, GetStatus, Mode, Normal, Pid, ProcessDown,
  Receiver, Resume, Running, Sender, Suspend, Suspended, SystemMessage,
}
import gleam/io
import gleam/atom
import gleam/result
import gleam/option.{Option}
import gleam/dynamic.{Dynamic}

type Message(message) {
  /// A regular message excepted by the process
  Message(message)

  /// An OTP system message, for debugging or maintenance
  System(SystemMessage)
}

/// The type used to indicate what to do after handling a message.
///
pub type Next(state) {
  /// Continue handling messages.
  ///
  Continue(state)

  /// Stop handling messages and shut down.
  ///
  Stop(ExitReason)
}

/// The type used to indicate whether an actor has started successfully or not.
///
pub type InitResult(state, message) {
  /// The actor has successfully initialised. The actor can start handling
  /// messages and actor's channel sender can be returned to the parent
  /// process.
  ///
  Ready(state: state, receiver: Option(Receiver(message)))

  /// The actor has failed to initialise. The actor shuts down and an error is
  /// returned to the parent process.
  ///
  Failed(ExitReason)
}

type Self(state, msg) {
  Self(
    pid: Pid,
    mode: Mode,
    parent: Pid,
    state: state,
    receiver: Receiver(Message(msg)),
    debug_state: DebugState,
    message_handler: fn(msg, state) -> Next(state),
  )
}

/// This data structure holds all the values required by the `start_spec`
/// function in order to create an actor.
///
/// If you do not need to configure the initialisation behaviour of your actor
/// consider using the `start` function.
///
pub type Spec(state, msg) {
  Spec(
    /// The initialisation functionality for the actor. This function is called
    /// just after the actor starts but before the channel sender is returned
    /// to the parent.
    ///
    /// This function is used to ensure that any required data or state is
    /// correct. If this function returns an error it means that the actor has
    /// failed to start and an error is returned to the parent.
    ///
    init: fn() -> InitResult(state, msg),
    /// How many milliseconds the `init` function has to return before it is
    /// considered to have taken too long and failed.
    ///
    init_timeout: Int,
    /// This function is called to handle each message that the actor receives.
    ///
    loop: fn(msg, state) -> Next(state),
  )
}

// TODO: Check needed functionality here to be OTP compatible
fn exit_process(reason: ExitReason) -> ExitReason {
  // TODO
  reason
}

fn receive_message(self: Self(state, msg)) -> Message(msg) {
  let system_receiver =
    process.system_receiver()
    |> process.map_receiver(System)

  let receiver = case self.mode {
    Suspended -> system_receiver
    Running ->
      self.receiver
      |> process.merge_receiver(system_receiver)
  }
  process.receive_forever(receiver)
}

fn process_status_info(self: Self(state, msg)) -> process.StatusInfo {
  process.StatusInfo(
    mod: atom.create_from_string("gleam@otp@actor"),
    parent: self.parent,
    mode: self.mode,
    debug_state: self.debug_state,
    state: dynamic.from(self.state),
  )
}

fn loop(self: Self(state, msg)) -> ExitReason {
  case receive_message(self) {
    System(GetState(caller)) -> {
      process.send(caller, dynamic.from(self.state))
      loop(self)
    }

    System(Resume(caller)) -> {
      process.send(caller, Nil)
      loop(Self(..self, mode: Running))
    }

    System(Suspend(caller)) -> {
      process.send(caller, Nil)
      loop(Self(..self, mode: Suspended))
    }

    System(GetStatus(caller)) -> {
      process.send(caller, process_status_info(self))
      loop(self)
    }

    Message(msg) ->
      case self.message_handler(msg, self.state) {
        Stop(reason) -> exit_process(reason)
        Continue(state) -> loop(Self(..self, state: state))
      }

    _unsupported_system_message -> {
      io.println("Gleam Action: unsupported system message dropped")
      loop(self)
    }
  }
}

fn merge_extra_receiver(r1, r2) {
  case r2 {
    option.None -> r1
    option.Some(r2) ->
      process.merge_receiver(r1, process.map_receiver(r2, Message))
  }
}

fn initialise_actor(
  spec: Spec(state, msg),
  ack_channel: Sender(Result(Sender(msg), ExitReason)),
) {
  let tuple(sender, receiver) = process.new_channel()
  let receiver = process.map_receiver(receiver, Message)
  case spec.init() {
    Ready(state, extra_receiver) -> {
      // Signal to parent that the process has initialised successfully
      process.send(ack_channel, Ok(sender))
      // Start message receive loop
      let self =
        Self(
          pid: process.self(),
          state: state,
          parent: process.pid(ack_channel),
          receiver: merge_extra_receiver(receiver, extra_receiver),
          message_handler: spec.loop,
          debug_state: process.debug_state([]),
          mode: Running,
        )
      loop(self)
    }

    Failed(reason) -> {
      process.send(ack_channel, Error(reason))
      exit_process(reason)
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
/// to a type compatible with Erlang supervisors see the `wrap_erlang*` functions
/// in the supervisor module.
///
pub type StartResult(msg) =
  Result(Sender(msg), StartError)

type StartInitMessage(msg) {
  Ack(Result(Sender(msg), ExitReason))
  Mon(ProcessDown)
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
pub fn start_spec(spec: Spec(state, msg)) -> Result(Sender(msg), StartError) {
  let tuple(ack_sender, ack_receiver) = process.new_channel()

  let child = process.start(fn() { initialise_actor(spec, ack_sender) })

  let receiver =
    ack_receiver
    |> process.map_receiver(Ack)
    |> process.merge_receiver(
      child
      |> process.monitor_process
      |> process.map_receiver(Mon),
    )

  case process.receive(receiver, spec.init_timeout) {
    // Child started OK
    Ok(Ack(Ok(channel))) -> {
      process.close_channels(receiver)
      Ok(channel)
    }

    // Child initialiser returned an error
    Ok(Ack(Error(reason))) -> {
      process.close_channels(receiver)
      Error(InitFailed(reason))
    }

    // Child went down while initialising
    Ok(Mon(down)) -> {
      process.close_channels(receiver)
      Error(InitCrashed(down.reason))
    }

    // Child did not finish initialising in time
    Error(Nil) -> {
      process.kill(child)
      process.close_channels(receiver)
      Error(InitTimeout)
    }
  }
}

/// Start an actor with a given initial state and message handling loop
/// function.
///
/// This function returns a `Result` but it will always be `Ok` so it is safe
/// to use with `assert` if you are not starting this actor as part of a
/// supervision tree.
///
/// If you wish to configure the initialisation behaviour of a new actor see
/// the `Spec` record and the `start_spec` function.
///
pub fn start(
  state: state,
  loop: fn(msg, state) -> Next(state),
) -> Result(Sender(msg), StartError) {
  start_spec(Spec(
    init: fn() { Ready(state, option.None) },
    loop: loop,
    init_timeout: 5000,
  ))
}

/// Send a message over a given channel.
///
/// This is a re-export of `process.send`, for the sake of convenience.
///
pub fn send(channel: Sender(msg), msg: msg) -> Sender(msg) {
  process.send(channel, msg)
}

// TODO: test
/// Send a synchronous message and wait for a response from the receiving
/// process.
///
/// If a reply is not received within the given timeout then the sender process
/// crashes. If you wish receive a `Result` rather than crashing see the
/// `process.try_call` function.
///
/// This is a re-export of `process.call`, for the sake of convenience.
///
pub fn call(
  receiver: Sender(message),
  make_message: fn(Sender(reply)) -> message,
  timeout: Int,
) -> reply {
  process.call(receiver, make_message, timeout)
}
