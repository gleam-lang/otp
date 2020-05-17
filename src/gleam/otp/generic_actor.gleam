import gleam/otp/process.{Pid, ExitReason, Self, StartResult, UnknownMessage, Ref, From, SystemResponse, SystemRequest}
import gleam/otp/port.{Port}
import gleam/result

pub type Next(state) {
  Continue(state)
  Stop(ExitReason)
}

// TODO: document
pub type Message(msg) {
  /// A regular message excepted by the process
  Message(message: msg)

  /// A linked process has exited and the current process is trapping exits
  Exit(pid: Pid(UnknownMessage), reason: ExitReason)

  /// A monitored process has exited
  ProcessDown(ref: Ref, pid: Pid(UnknownMessage), reason: ExitReason)

  /// A monitored port has exited
  PortDown(ref: Ref, port: Port, reason: ExitReason)
}

type Handler(msg, state) =
  fn(Message(msg), state) -> Next(state)

pub type Spec(state, msg) {
  Spec(
    init: fn(Pid(msg)) -> Result(state, ExitReason),
    loop: fn(Message(msg), state) -> Next(state),
  )
}

// TODO: Check needed functionality here
fn exit_process(_: ExitReason) -> Nil {
  Nil
}

fn convert_message(
  msg: process.Message(msg),
) -> Result(Message(msg), tuple(From(SystemResponse), SystemRequest)) {
  case msg {
    process.System(from, request) -> Error(tuple(from, request))
    process.Exit(pid, reason) -> Ok(Exit(pid, reason))
    process.Message(msg) -> Ok(Message(msg))
    process.ProcessDown(ref, pid, reason) -> Ok(ProcessDown(ref, pid, reason))
    process.PortDown(ref, port, reason) -> Ok(PortDown(ref, port, reason))
  }
}

fn loop(self: Self(msg), handler: Handler(msg, state), state: state) -> Nil {
  // TODO: Wait forever
  case process.receive(self, 1000000) {
    Error(Nil) -> loop(self, handler, state)
    Ok(msg) -> case convert_message(msg) {
      // TODO: handle system message
      Error(tuple(_from, _request)) -> loop(self, handler, state)
      Ok(msg) -> case handler(msg, state) {
        Stop(reason) -> exit_process(reason)
        Continue(state) -> loop(self, handler, state)
      }
    }
  }
}

pub fn start(spec: Spec(state, msg)) -> StartResult(msg) {
  process.start(
    fn(self) {
      let Spec(init: init, ..) = spec
      let Self(pid: pid, ..) = self
      case init(pid) {
        Ok(state) -> loop(self, spec.loop, state)
        Error(reason) -> exit_process(reason)
      }
    },
  )
}

pub fn async_send(to receiver: Pid(msg), msg msg: msg) -> Nil {
  process.async_send(receiver, msg)
}

pub fn sync_send(
  to receiver: Pid(msg),
  message msg: fn(From(reply)) -> msg,
  timeout timeout: Int,
) -> reply {
  process.sync_send(receiver, msg, timeout)
}
