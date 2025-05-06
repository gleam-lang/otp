//// Bindings to Erlang/OTP's `supervisor` module.
////
//// For further detail see the Erlang documentation:
//// <https://www.erlang.org/doc/apps/stdlib/supervisor.html>.
////
//// # Example
////
//// ```gleam
//// import gleam/erlang/actor
//// import gleam/otp/static_supervisor.{type Supervisor} as supervisor
//// import app/database_pool
//// import app/http_server
//// 
//// pub fn start_supervisor() ->  {
////   supervisor.new(supervisor.OneForOne)
////   |> supervisor.add(database_pool.specification())
////   |> supervisor.add(http_server.specification())
////   |> supervisor.start
//// }
//// ```

import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}

pub type Supervisor {
  Supervisor(pid: Pid)
}

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

pub opaque type Builder {
  Builder(
    strategy: Strategy,
    intensity: Int,
    period: Int,
    auto_shutdown: AutoShutdown,
    children: List(ChildSpecification(Nil)),
  )
}

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
pub fn auto_shutdown(builder: Builder, value: AutoShutdown) -> Builder {
  Builder(..builder, auto_shutdown: value)
}

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
    [dynamic.from(child.start)],
  )

  let #(type_, shutdown) = case child.child_type {
    supervision.Supervisor -> #(
      atom.create("supervisor"),
      dynamic.from(atom.create("infinity")),
    )
    supervision.Worker(timeout) -> #(
      atom.create("worker"),
      dynamic.from(timeout),
    )
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
fn make_erlang_start_flags(flags: List(ErlangStartFlag)) -> ErlangStartFlags

type ErlangStartFlag {
  Strategy(Strategy)
  Intensity(Int)
  Period(Int)
  AutoShutdown(AutoShutdown)
}

type ErlangChildSpec

@external(erlang, "maps", "from_list")
fn make_erlang_child_spec(
  properties: List(ErlangChildSpecProperty),
) -> ErlangChildSpec

type ErlangChildSpecProperty {
  Id(Int)
  Start(#(Atom, Atom, List(Dynamic)))
  Restart(supervision.Restart)
  Significant(Bool)
  Type(Atom)
  Shutdown(Dynamic)
}

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
