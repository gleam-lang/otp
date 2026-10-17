//// In the context of the OTP framework an "application" is a collection of
//// code that can be loaded into the virtual machine. Each Gleam package is a
//// single OTP application.
////
//// One feature of OTP applications that makes them different from packages or
//// libraries in other languages is that they have the option of defining a
//// module through which they can be _started_ and _stopped_, and they can be
//// configured using Erlang's configuration system.
////
//// ## OTP application programs
////
//// Long running Gleam programs (such as backend web applications) typically
//// want to define an application module, and to use it as the entrypoint for
//// the program in favour of the `main` function.
////
//// ## OTP application libraries
////
//// It is always preferred for libraries to not be stateful! Instead they
//// should expose functions for Gleam apps to call, passing configuration as
//// arguments. There may be some libraries for which it makes sense to have
//// this implicit global mutable state, but they are very rare.
////
//// ## Usage
////
////

// TODO: give example of how to use it

import gleam
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node
import gleam/erlang/process
import gleam/otp/actor

/// A recipe of how to start the stateful OTP application.
///
/// See the module documentation for how to use this type in your program.
///
pub opaque type Application(state) {
  Application(
    start: fn(StartType) -> actor.StartResult(state),
    before_stop: fn(state) -> state,
    after_stop: fn(state) -> Nil,
  )
}

// TODO: test
/// Create a new application recipe from a starter function. This function is
/// called whenever an application is started, and it starts the supervision tree
/// of the OTP application.
/// 
/// The `actor.StartResult` data returned from the starter function is used as the 
/// state of the application and will be passed to the `before_stop` and
/// `after_stop` callbacks when the application is stopped.
/// 
pub fn new(
  start: fn(StartType) -> actor.StartResult(state),
) -> Application(state) {
  Application(
    before_stop: fn(state) { state },
    after_stop: fn(_state) { Nil },
    start:,
  )
}

// TODO: test
/// Configure the application with a callback function to be run before the
/// application is stopped. This callback function can modify the application's
/// state value, which will then be passed to the `after_stop` callback.
///
/// This is a best-effort API! There is no guarentee that this function will be
/// called before an application stops, for example, it likely may not be
/// called if the VM crashes.
///
pub fn before_stop(
  application: Application(state),
  before_stop: fn(state) -> state,
) -> Application(state) {
  Application(..application, before_stop:)
}

// TODO: test
/// Configure the application with a callback function to be run after the
/// application is stopped.
///
/// This is a best-effort API! There is no guarentee that this function will be
/// called after an application stops, for example, it likely may not be
/// called if the VM crashes.
///
pub fn after_stop(
  application: Application(state),
  after_stop: fn(state) -> Nil,
) -> Application(state) {
  Application(..application, after_stop:)
}

/// A value of this type is passed as an argument to a stateful OTP
/// application's when it starts, to indicate the context in which the
/// application been started.
///
pub type StartType {
  /// The application is starting normally.
  Normal
  /// The application is distributed and started at the current node because of
  /// a takeover from the other node.
  Takeover(node.Node)
  /// The application is distributed and started at the current node because of
  /// a failover from the other node, and the application is configured with
  /// "start phases". See the Erlang/OTP application documentation for more
  /// information.
  /// <https://www.erlang.org/doc/apps/kernel/application.html#c:start/2>
  Failover(node.Node)
}

//
// OTP application callbacks
//

/// <https://www.erlang.org/doc/apps/kernel/application.html#c:start/2>
///
/// ```erlang
/// -callback start(StartType :: start_type(), StartArgs :: term()) ->
///                    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
/// ```
///
@internal
pub fn start(
  start_type: StartType,
  application_module: Atom,
) -> ErlangResult2(process.Pid, state, actor.StartError) {
  let application: Application(state) =
    apply(application_module, atom.create("main"), [])
  case application.start(start_type) {
    gleam.Ok(started) -> Ok(started.pid, started.data)
    gleam.Error(error) -> Error(error)
  }
}

/// <https://www.erlang.org/doc/apps/kernel/application.html#c:pre_stop/1>
///
/// ```erlang
/// -callback prep_stop(State) -> NewState when State :: term(), NewState :: term().
/// ```
///
@internal
pub fn prep_stop(
  state: #(Application(state), state),
) -> #(Application(state), state) {
  let #(application, state) = state
  let state = application.before_stop(state)
  #(application, state)
}

/// <https://www.erlang.org/doc/apps/kernel/application.html#c:stop/1>
///
/// ```erlang
/// -callback stop(State :: term()) -> term().
/// ```
///
@internal
pub fn stop(state: #(Application(state), state)) -> Nil {
  let #(application, state) = state
  application.after_stop(state)
}

@internal
pub type ErlangResult2(data1, data2, error) {
  Ok(data1, data2)
  Error(error)
}

@external(erlang, "erlang", "apply")
fn apply(module: Atom, function: Atom, arguments: List(argument)) -> returned
