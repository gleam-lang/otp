//// TODO: Check needed functionality here to be OTP compatible
//// TODO: document
//// TODO: document
//// TODO: test
////pub fn async_send(to receiver: Pid, msg msg: msg) -> Nil {
////  process.async_send(receiver, msg)
////}
//// TODO: document
//// TODO: test
////pub fn sync_send(
////  to receiver: Pid,
////  message msg: fn(From(reply)) -> msg,
////  timeout timeout: Int,
////) -> reply {
////  process.sync_send(receiver, msg, timeout)
////}


//import gleam/otp/process.{
//  DebugState, ExitReason, From, GetState, GetStatus, Message, Normal, Pid, Ref,
//  Resume, StartResult, Suspend, System, SystemMessage,
//}
//import gleam/otp/port.{Port}
//import gleam/result
//import gleam/atom
//import gleam/dynamic.{Dynamic}
//pub type Next(state) {
//  Continue(state)
//  Stop(ExitReason)
//}
//type Mode {
//  Running
//  Suspended
//}
//type Self {
//  Self(pid: Pid, parent: Pid)
//}
//pub type Spec(state, msg) {
//  Spec(
//    init: fn() -> Result(state, ExitReason),
//    loop: fn(msg, state) -> Next(state),
//  )
//}
//fn exit_process(reason: ExitReason) -> ExitReason {
//  // TODO
//  reason
//}
//fn actor_status(
//  self: Self,
//  debug: DebugState,
//  mode: Mode,
//  state: state,
//) -> Dynamic {
//  tuple(
//    atom.create_from_string("status"),
//    self.pid,
//    tuple(
//      atom.create_from_string("module"),
//      atom.create_from_string("gleam@otp@actor"),
//    ),
//    [
//      dynamic.from([]),
//      dynamic.from(mode),
//      dynamic.from(self.parent),
//      dynamic.from(debug),
//      dynamic.from(tuple(atom.create_from_string("state"), state)),
//    ],
//  )
//  |> dynamic.from
//}
//fn loop(
//  self: Self,
//  handler: fn(msg, state) -> Next(state),
//  state: state,
//  debug: DebugState,
//  mode: Mode,
//) -> ExitReason {
//  let msg = case mode {
//    // System(process.receive_system_forever())
//    Suspended -> todo
//    // process.receive_forever(self)
//    Running -> todo
//  }
//  case msg {
//    System(GetState(from)) -> {
//      process.reply(to: from, with: dynamic.from(state))
//      loop(self, handler, state, debug, mode)
//    }
//    System(Resume(from)) -> {
//      process.reply(to: from, with: Nil)
//      loop(self, handler, state, debug, Running)
//    }
//    System(Suspend(from)) -> {
//      process.reply(to: from, with: Nil)
//      loop(self, handler, state, debug, Suspended)
//    }
//    System(GetStatus(from)) -> {
//      process.reply(to: from, with: actor_status(self, debug, mode, state))
//      loop(self, handler, state, debug, mode)
//    }
//    System(_msg) -> todo("This system message is not yet supported")
//    Message(msg) -> case handler(msg, state) {
//      Stop(reason) -> exit_process(reason)
//      Continue(state) -> loop(self, handler, state, debug, mode)
//    }
//  }
//}
//pub fn start(spec: Spec(state, msg)) -> StartResult {
//  let parent = process.self()
//  let routine = fn() {
//    let self = Self(parent: parent, pid: process.self())
//    case spec.init() {
//      Ok(state) -> {
//        // TODO
//        // process.started(self)
//        let debug = process.debug_state([])
//        loop(self, spec.loop, state, debug, Running)
//      }
//      Error(reason) -> exit_process(reason)
//    }
//  }
//  // TODO: ensure init succeeds
//  Ok(process.start(routine))
//}
