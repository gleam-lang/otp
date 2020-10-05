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

pub type Next(state) {
  Continue(state)
  Stop(ExitReason)
}

pub type InitResult(state, message) {
  Ready(state: state, receiver: Option(Receiver(message)))
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

pub type Spec(state, msg) {
  Spec(
    init: fn() -> InitResult(state, msg),
    loop: fn(msg, state) -> Next(state),
    init_timeout: Int,
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

    _other -> todo("message not yet supported")
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

type StartInitMessage(msg) {
  Ack(Result(Sender(msg), ExitReason))
  Mon(ProcessDown)
}

// TODO: document
// TODO: test init_timeout. Currently if we test it eunit prints an error from
// the process death. How do we avoid this?
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
      // TODO: Flush exit messages as we may be trapping exits
      Error(InitTimeout)
    }
  }
}

// TODO: document
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

// TODO: document
// TODO: test
// TODO: document
pub fn send(channel: Sender(msg), msg: msg) -> Sender(msg) {
  process.send(channel, msg)
}

// TODO: document
// TODO: test
pub fn call(
  receiver: Sender(message),
  make_message: fn(Sender(reply)) -> message,
  timeout: Int,
) -> reply {
  process.call(receiver, make_message, timeout)
}
