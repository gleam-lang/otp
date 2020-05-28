import gleam/otp/process.{Pid, ExitReason, Self, StartResult, UnknownMessage, Ref, From, Normal, System, Message, GetState, Suspend, Resume, SystemMessage, GetStatus}
import gleam/otp/port.{Port}
import gleam/result
import gleam/atom
import gleam/dynamic.{Dynamic}

pub type Next(state) {
  Continue(state)
  Stop(ExitReason)
}

type Mode {
  Running
  Suspended
}

pub type Spec(state, msg) {
  Spec(
    init: fn(Pid(msg)) -> Result(state, ExitReason),
    loop: fn(msg, state) -> Next(state),
  )
}

// TODO: Check needed functionality here to be OTP compatible
fn exit_process(reason: ExitReason) -> ExitReason {
  // TODO
  reason
}

fn actor_status(self: Self(msg), mode: Mode, state: state) -> Dynamic {
  tuple(
    atom.create_from_string("status"),
    self.pid,
    tuple(
      atom.create_from_string("module"),
      atom.create_from_string("gleam@otp@actor"),
    ),
    [
      dynamic.from([]),
      dynamic.from(mode),
      dynamic.from(self.parent),
      dynamic.from(self.debug),
      dynamic.from(tuple(atom.create_from_string("state"), state)),
    ],
  )
  |> dynamic.from
}

fn loop(
  self: Self(msg),
  handler: fn(msg, state) -> Next(state),
  state: state,
  mode: Mode,
) -> ExitReason {
  let msg = case mode {
    Suspended -> System(process.receive_system_forever())
    Running -> process.receive_forever(self)
  }

  case msg {
    System(GetState(from)) -> {
      process.reply(to: from, with: dynamic.from(state))
      loop(self, handler, state, mode)
    }

    System(Resume(from)) -> {
      process.reply(to: from, with: Nil)
      loop(self, handler, state, Running)
    }

    System(Suspend(from)) -> {
      process.reply(to: from, with: Nil)
      loop(self, handler, state, Suspended)
    }

    System(GetStatus(from)) -> {
      process.reply(to: from, with: actor_status(self, mode, state))
      loop(self, handler, state, mode)
    }

    System(_msg) -> todo

    Message(msg) -> case handler(msg, state) {
      Stop(reason) -> exit_process(reason)
      Continue(state) -> loop(self, handler, state, mode)
    }
  }
}

// TODO: document
// TODO: test
pub fn start(spec: Spec(state, msg)) -> StartResult(msg) {
  let routine = fn(self: Self(msg)) {
    case spec.init(self.pid) {
      Ok(state) -> {
        process.started(self)
        loop(self, spec.loop, state, Running)
      }
      Error(reason) -> exit_process(reason)
    }
  }
  process.start(routine)
}

// TODO: document
// TODO: test
pub fn async_send(to receiver: Pid(msg), msg msg: msg) -> Nil {
  process.async_send(receiver, msg)
}

// TODO: document
// TODO: test
pub fn sync_send(
  to receiver: Pid(msg),
  message msg: fn(From(reply)) -> msg,
  timeout timeout: Int,
) -> reply {
  process.sync_send(receiver, msg, timeout)
}
