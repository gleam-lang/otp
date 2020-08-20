////
//// Basic and rather sub-optimal bindings to OTP's supervisor module.
//// Enough to enable us to get started making supervision trees, but that's
//// about it.
////
//// Later we shall make better supervisor creation modules that are safer and
//// nicer to use.
////
//// nodoc
//// nodoc
//// nodoc
//// nodoc


//import gleam/atom.{Atom}
//import gleam/dynamic
//import gleam/list
//import gleam/otp/process.{Pid}
//import gleam/result
//pub type Strategy {
//  OneForOne
//  OneForAll
//  RestForOne
//}
//pub type Restart {
//  Permanent
//  Transient
//  Temporary
//}
//pub type ChildSpec {
//  SupervisorSpec(id: String, start: fn() -> Result(Pid, String))
//  WorkerSpec(
//    id: String,
//    start: fn() -> Result(Pid, String),
//    restart: Restart,
//    shutdown: Int,
//  )
//}
//pub type Spec {
//  Spec(
//    strategy: Strategy,
//    intensity: Int,
//    period: Int,
//    children: List(ChildSpec),
//  )
//}
//external fn erl_start_link(Atom, Spec) -> Result(Pid, String) =
//  "supervisor" "start_link"
//pub fn start_link(spec: Spec) -> Result(Pid, String) {
//  erl_start_link(atom.create_from_string("gleam@otp@basic_supervisor"), spec)
//}
//pub type Modules {
//  Dynamic
//}
//pub fn call(f) {
//  f()
//}
//pub type ChildType {
//  Supervisor
//  Worker
//}
//type InfinityTimeout {
//  Infinity
//}
//pub fn init(
//  spec: Spec,
//) -> Result(
//  tuple(
//    tuple(Strategy, Int, Int),
//    List(
//      tuple(
//        String,
//        tuple(Atom, Atom, List(dynamic.Dynamic)),
//        Restart,
//        Int,
//        ChildType,
//        Modules,
//      ),
//    ),
//  ),
//  String,
//) {
//  // id
//  // start
//  // restart
//  // shutdown
//  // type
//  // modules
//  let Spec(strategy, intensity, period, children) = spec
//  let mod = atom.create_from_string("gleam@otp@basic_supervisor")
//  let mod_fn = atom.create_from_string("call")
//  let mfa = fn(start) { tuple(mod, mod_fn, [dynamic.from(start)]) }
//  Ok(
//    tuple(
//      tuple(strategy, intensity, period),
//      list.map(
//        children,
//        fn(child_spec) {
//          case child_spec {
//            WorkerSpec(
//              id,
//              start,
//              restart,
//              shutdown,
//            ) -> tuple(id, mfa(start), restart, shutdown, Worker, Dynamic)
//            SupervisorSpec(
//              id,
//              start,
//            ) -> tuple(
//              id,
//              mfa(start),
//              Permanent,
//              dynamic.unsafe_coerce(dynamic.from(Infinity)),
//              Supervisor,
//              Dynamic,
//            )
//          }
//        },
//      ),
//    ),
//  )
//}
