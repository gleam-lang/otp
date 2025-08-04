//// Bindings to Erlang/OTP's `supervisor` module.
////
//// For further detail see the Erlang documentation:
//// <https://www.erlang.org/doc/apps/stdlib/supervisor.html>.
////
//// # Example
////
//// ```gleam
//// import gleam/otp/actor
//// import gleam/otp/static_supervisor.{type Supervisor} as supervisor
//// import app/database_pool
//// import app/http_server
//// 
//// pub fn start_supervisor() -> actor.StartResult(Supervisor) {
////   supervisor.new(supervisor.OneForOne)
////   |> supervisor.add(database_pool.supervised())
////   |> supervisor.add(http_server.supervised())
////   |> supervisor.start
//// }
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

/// A reference to the running supervisor. In future this could be used to send
/// commands to the supervisor to perform certain actions, but today no such
/// APIs have been exposed.
///
/// This supervisor wrap Erlang/OTP's `supervisor` module, and as such it does
/// not use subjects for message sending. If it was implemented in Gleam a
/// subject might be used instead of this type.
///
pub opaque type Supervisor {
  Supervisor(pid: Pid)
}

/// How the supervisor should react when one of its children terminates.
pub type Strategy {
  /// If one child process terminates and is to be restarted, only that child
  /// process is affected. This is the default restart strategy.
  OneForOne

  /// If one child process terminates and is to be restarted, all other child
  /// processes are terminated and then all child processes are restarted.
  OneForAll

  /// If one child process terminates and is to be restarted, the 'rest' of the
  /// child processes (that is, the child processes after the terminated child
  /// process in the start order) are terminated. Then the terminated child
  /// process and all child processes after it are restarted.
  RestForOne
}

/// A supervisor can be configured to automatically shut itself down with exit
/// reason shutdown when significant children terminate with the auto_shutdown
/// key in the above map.
pub type AutoShutdown {
  /// Automic shutdown is disabled. This is the default setting.
  ///
  /// With auto_shutdown set to never, child specs with the significant flag
  /// set to true are considered invalid and will be rejected.
  Never
  /// The supervisor will shut itself down when any significant child
  /// terminates, that is, when a transient significant child terminates
  /// normally or when a temporary significant child terminates normally or
  /// abnormally.
  AnySignificant
  /// The supervisor will shut itself down when all significant children have
  /// terminated, that is, when the last active significant child terminates.
  /// The same rules as for any_significant apply.
  AllSignificant
}

/// A builder for configuring and starting a supervisor. See each of the
/// functions that take this type for details of the configuration possible.
///
/// # Example
///
/// ```gleam
/// import gleam/erlang/actor
/// import gleam/otp/static_supervisor.{type Supervisor} as supervisor
/// import app/database_pool
/// import app/http_server
/// 
/// pub fn start_supervisor() ->  {
///   supervisor.new(supervisor.OneForOne)
///   |> supervisor.add(database_pool.supervised())
///   |> supervisor.add(http_server.supervised())
///   |> supervisor.start
/// }
/// ```
///
pub opaque type Builder {
  Builder(
    strategy: Strategy,
    intensity: Int,
    period: Int,
    auto_shutdown: AutoShutdown,
    children: List(ChildSpecification(Nil)),
  )
}

/// Create a new supervisor builder, ready for further configuration.
///
pub fn new(strategy strategy: Strategy) -> Builder {
  Builder(
    strategy: strategy,
    intensity: 2,
    period: 5,
    auto_shutdown: Never,
    children: [],
  )
}

/// To prevent a supervisor from getting into an infinite loop of child
/// process terminations and restarts, a maximum restart intensity is
/// defined using two integer values specified with keys intensity and
/// period in the above map. Assuming the values MaxR for intensity and MaxT
/// for period, then, if more than MaxR restarts occur within MaxT seconds,
/// the supervisor terminates all child processes and then itself. The
/// termination reason for the supervisor itself in that case will be
/// shutdown. 
///
/// Intensity defaults to 1 and period defaults to 5.
pub fn restart_tolerance(
  builder: Builder,
  intensity intensity: Int,
  period period: Int,
) -> Builder {
  Builder(..builder, intensity: intensity, period: period)
}

/// A supervisor can be configured to automatically shut itself down with
/// exit reason shutdown when significant children terminate.
///
pub fn auto_shutdown(builder: Builder, value: AutoShutdown) -> Builder {
  Builder(..builder, auto_shutdown: value)
}

/// Start a new supervisor process with the configuration and children
/// specified within the builder.
///
/// Typically you would use the `supervised` function to add your supervisor to
/// a supervision tree instead of using this function directly.
///
/// The supervisor will be linked to the parent process that calls this
/// function.
///
/// If any child fails to start the supevisor first terminates all already
/// started child processes with reason shutdown and then terminate itself and
/// returns an error.
///
pub fn start(
  builder: Builder,
) -> Result(actor.Started(Supervisor), actor.StartError) {
  let flags =
    make_erlang_start_flags([
      Strategy(builder.strategy),
      Intensity(builder.intensity),
      Period(builder.period),
      AutoShutdown(builder.auto_shutdown),
    ])

  let module = atom.create("gleam@otp@static_supervisor")
  let children =
    builder.children |> list.reverse |> list.index_map(convert_child)
  case erlang_start_link(module, #(flags, children)) {
    Ok(pid) -> Ok(actor.Started(pid:, data: Supervisor(pid)))
    Error(error) -> Error(convert_erlang_start_error(error))
  }
}

/// Create a `ChildSpecification` that adds this supervisor as the child of
/// another, making it fault tolerant and part of the application's supervision
/// tree. You should prefer to starting unsupervised supervisors with the
/// `start` function.
///
/// If any child fails to start the supevisor first terminates all already
/// started child processes with reason shutdown and then terminate itself and
/// returns an error.
///
pub fn supervised(builder: Builder) -> ChildSpecification(Supervisor) {
  supervision.supervisor(fn() { start(builder) })
}

@external(erlang, "gleam_otp_external", "convert_erlang_start_error")
fn convert_erlang_start_error(dynamic: Dynamic) -> actor.StartError

@external(erlang, "supervisor", "start_link")
fn erlang_start_link(
  module: Atom,
  args: #(ErlangStartFlags, List(ErlangChildSpec)),
) -> Result(Pid, Dynamic)

/// Add a child to the supervisor.
pub fn add(builder: Builder, child: ChildSpecification(data)) -> Builder {
  Builder(..builder, children: [
    supervision.map_data(child, fn(_) { Nil }),
    ..builder.children
  ])
}

fn convert_child(child: ChildSpecification(data), id: Int) -> ErlangChildSpec {
  let mfa = #(
    atom.create("gleam@otp@static_supervisor"),
    atom.create("start_child_callback"),
    [child.start],
  )

  let #(type_, shutdown) = case child.child_type {
    supervision.Supervisor -> #(atom.create("supervisor"), make_timeout(-1))
    supervision.Worker(ms) -> #(atom.create("worker"), make_timeout(ms))
  }

  make_erlang_child_spec([
    Id(id),
    Start(mfa),
    Restart(child.restart),
    Significant(child.significant),
    Type(type_),
    Shutdown(shutdown),
  ])
}

type ErlangStartFlags

@external(erlang, "maps", "from_list")
fn make_erlang_start_flags(
  flags: List(ErlangStartFlag(data)),
) -> ErlangStartFlags

type ErlangStartFlag(data) {
  Strategy(Strategy)
  Intensity(Int)
  Period(Int)
  AutoShutdown(AutoShutdown)
}

type ErlangChildSpec

@external(erlang, "maps", "from_list")
fn make_erlang_child_spec(
  properties: List(ErlangChildSpecProperty(data)),
) -> ErlangChildSpec

type ErlangChildSpecProperty(data) {
  Id(Int)
  Start(
    #(Atom, Atom, List(fn() -> Result(actor.Started(data), actor.StartError))),
  )
  Restart(supervision.Restart)
  Significant(Bool)
  Type(Atom)
  Shutdown(Timeout)
}

type Timeout

/// Negative numbers mean an infinite timeout
@external(erlang, "gleam_otp_external", "make_timeout")
fn make_timeout(amount: Int) -> Timeout

// Callback used by the Erlang supervisor module.
@internal
pub fn init(start_data: Dynamic) -> Result(Dynamic, never) {
  Ok(start_data)
}

// Callback used by the Erlang supervisor module.
@internal
pub fn start_child_callback(
  start: fn() -> Result(actor.Started(anything), actor.StartError),
) -> Result(Pid, actor.StartError) {
  case start() {
    Ok(started) -> Ok(started.pid)
    Error(error) -> Error(error)
  }
}
