import gleam
import gleam/erlang/atom.{type Atom}
import gleam/erlang/node
import gleam/erlang/process
import gleam/otp/actor

// TODO: document
pub opaque type Application(state) {
  Application(
    start: fn(StartType) -> actor.StartResult(state),
    before_stop: fn(state) -> state,
    after_stop: fn(state) -> Nil,
  )
}

// TODO: document
pub type StartType {
  Normal
  Takeover(node.Node)
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
pub fn pre_stop(
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
