import gleam/otp/process.{Pid, ExitReason, Self, StartResult, UnknownMessage, Ref, From, Normal, System, Message, GetState}
import gleam/otp/port.{Port}
import gleam/result
import gleam/dynamic

pub type Next(state) {
  Continue(state)
  Stop(ExitReason)
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

fn loop(
  self: Self(msg),
  handler: fn(msg, state) -> Next(state),
  state: state,
) -> ExitReason {
  case process.receive_forever(self) {
    System(GetState(from)) -> {
      process.reply(to: from, with: dynamic.from(state))
      loop(self, handler, state)
    }

    System(_msg) -> todo

    Message(msg) -> case handler(msg, state) {
      Stop(reason) -> exit_process(reason)
      Continue(state) -> loop(self, handler, state)
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
        loop(self, spec.loop, state)
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
