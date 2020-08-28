import gleam/otp/process.{
  Abnormal, Channel, DebugState, ExitReason, GetState, GetStatus, Mode, Normal, Pid,
  ProcessDown, Resume, Running, Suspend, Suspended, SystemMessage,
}
import gleam/result
import gleam/atom
import gleam/dynamic.{Dynamic}

// TODO: document
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

type Self(state, msg) {
  Self(
    pid: Pid,
    mode: Mode,
    parent: Pid,
    state: state,
    channel: Channel(msg),
    debug_state: DebugState,
    message_handler: fn(msg, state) -> Next(state),
  )
}

pub type Spec(state, msg) {
  Spec(
    init: fn() -> Result(state, ExitReason),
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
  let receiver =
    process.new_receiver()
    |> process.remove_timeout
    |> process.include_system(System)

  let receiver = case self.mode {
    Running -> process.include_channel(receiver, self.channel, Message)
    Suspended -> receiver
  }
  process.run_receiver_forever(receiver)
}

fn set_mode(self: Self(state, msg), mode: Mode) -> Self(state, msg) {
  Self(
    pid: self.pid,
    state: self.state,
    parent: self.parent,
    channel: self.channel,
    message_handler: self.message_handler,
    debug_state: self.debug_state,
    mode: mode,
  )
}

fn set_state(self: Self(state, msg), state: state) -> Self(state, msg) {
  Self(
    pid: self.pid,
    state: state,
    parent: self.parent,
    channel: self.channel,
    message_handler: self.message_handler,
    debug_state: self.debug_state,
    mode: self.mode,
  )
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
      self
      |> set_mode(Running)
      |> loop
    }

    System(Suspend(caller)) -> {
      process.send(caller, Nil)
      self
      |> set_mode(Suspended)
      |> loop
    }

    System(GetStatus(caller)) -> {
      process.send(caller, process_status_info(self))
      loop(self)
    }

    Message(msg) ->
      case self.message_handler(msg, self.state) {
        Stop(reason) -> exit_process(reason)
        Continue(state) -> loop(set_state(self, state))
      }

    _other -> todo("message not yet supported")
  }
}

fn initialise_actor(
  spec: Spec(state, msg),
  ack_channel: Channel(Result(Channel(msg), ExitReason)),
) {
  let channel = process.new_channel()
  case spec.init() {
    Ok(state) -> {
      // Signal to parent that the process has initialised successfully
      process.send(ack_channel, Ok(channel))
      // Start message receive loop
      let self =
        Self(
          pid: process.self(),
          state: state,
          parent: process.pid(ack_channel),
          channel: channel,
          message_handler: spec.loop,
          debug_state: process.debug_state([]),
          mode: Running,
        )
      loop(self)
    }
    Error(reason) -> {
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
  Ack(Result(Channel(msg), ExitReason))
  Mon(ProcessDown)
}

// TODO: document
// TODO: test init_timeout. Currently if we test it eunit prints an error from
// the process death. How do we avoid this?
pub fn start(spec: Spec(state, msg)) -> Result(Channel(msg), StartError) {
  let ack = process.new_channel()

  let child = process.start(fn() { initialise_actor(spec, ack) })
  let monitor = process.monitor_process(child)

  let receiver =
    process.new_receiver()
    |> process.set_timeout(spec.init_timeout)
    |> process.include_channel(ack, Ack)
    |> process.include_process_monitor(monitor, Mon)

  case process.run_receiver(receiver) {
    // Child started OK
    Ok(Ack(Ok(channel))) -> {
      process.close_channel(ack)
      Ok(channel)
    }

    // Child initialiser returned an error
    Ok(Ack(Error(reason))) -> {
      process.close_channel(ack)
      Error(InitFailed(reason))
    }

    // Child went down while initialising
    Ok(Mon(down)) -> {
      process.demonitor_process(monitor)
      process.close_channel(ack)
      Error(InitCrashed(down.reason))
    }

    // Child did not finish initialising in time
    Error(Nil) -> {
      process.demonitor_process(monitor)
      process.kill(child)
      process.close_channel(ack)
      // Flush exit messages as we may be trapping exits
      process.new_receiver()
      |> process.include_process_exit(child, fn(x) { x })
      |> process.flush_receiver
      Error(InitTimeout)
    }
  }
}

// TODO: document
// TODO: test
// TODO: document
pub fn send(channel: Channel(msg), msg: msg) -> Channel(msg) {
  process.send(channel, msg)
}

// TODO: document
// TODO: test
pub fn call(
  receiver: Channel(message),
  make_message: fn(Channel(reply)) -> message,
  timeout: Int,
) -> reply {
  process.call(receiver, make_message, timeout)
}
