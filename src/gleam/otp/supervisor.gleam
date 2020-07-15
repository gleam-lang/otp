// TODO: test
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/otp/process.{ExitReason, OpaquePid, Pid, StartResult}

// API
type Children(state) {
  Children(pids: List(OpaquePid), state: state, init: Initialiser(Nil, state))
}

pub opaque type Spec(state) {
  Starting(Children(state))
  Failed(ExitReason)
}

pub opaque type ChildSpec(msg, state_in, state_out) {
  ChildSpec(
    start: fn(state_in) -> StartResult(msg),
    merge: fn(state_in, Pid(msg)) -> state_out,
  )
}

type InitialiserResult(state) =
  Result(tuple(state_out, List(OpaquePid)), ExitReason)

type Initialiser(state_in, state_out) =
  fn(
    state_in,
    Option(OpaquePid),
  ) -> Result(tuple(state_out, List(OpaquePid)), ExitReason)

fn start_initializer(
  child_spec: ChildSpec(msg, state_in, state_out),
  pids: List(OpaquePid),
  state: state_in,
  problem_pid: Option(OpaquePid),
) -> InitialiserResult(state_out) {
  case child_spec.start(state) {
    Error(fail) -> Error(fail)
    Ok(new_pid) -> {
      // If we have found the problem_pid then we don't need to keep looking
      let opaque_pid = process.make_opaque(new_pid)
      let problem_pid = case problem_pid {
        Some(pid) if opaque_pid != pid -> problem_pid
        _ -> None
      }
      let new_state = child_spec.merge(state, new_pid)
      Ok(tuple(new_state, [process.make_opaque(new_pid), ..pids]))
    }
  }
}

fn run_initializer(
  state: state_1,
  pids: List(OpaquePid),
  child_spec: ChildSpec(msg, state_1, state_2),
  problem_pid: Option(OpaquePid),
  existing_pid: Pid(msg),
  existing_state: state_2,
) -> InitialiserResult(state_2) {
  let opaque_pid = process.make_opaque(existing_pid)
  case problem_pid {
    // This Pid is still alive and we have not found the problem pid yet.
    // In this case we don't need to restart it, we keep the existing pid
    // and continue to the next.
    Some(problem) if problem != opaque_pid -> {
      let pids = [opaque_pid, ..pids]
      Ok(tuple(existing_state, pids))
    }

    // This pid either is the cause of the problem, or we don't have problem
    // pid to compare with. In either case it must be restarted.
    _ -> {
      process.send_exit(existing_pid, process.Normal)
      start_initializer(child_spec, pids, state, problem_pid)
    }
  }
}

fn add_initialiser_layer(
  init: Initialiser(state_0, state_1),
  child_spec: ChildSpec(msg, state_1, state_2),
  existing_pid: Pid(msg),
  existing_state: state_2,
) -> Initialiser(state_0, state_2) {
  fn(state, problem_pid) {
    case init(state, problem_pid) {
      // Older siblings failed, exit early
      Error(fail) -> Error(fail)

      // Older siblings ok, initialise the new process
      Ok(tup) -> {
        let tuple(state, pids) = tup
        run_initializer(
          state,
          pids,
          child_spec,
          problem_pid,
          existing_pid,
          existing_state,
        )
      }
    }
  }
}

pub fn begin() -> Spec(Nil) {
  Children(state: Nil, pids: [], init: fn(state, _) { Ok(tuple(state, [])) })
  |> Starting
}

pub fn add(
  spec: Spec(state),
  child_spec: ChildSpec(msg, state, new_state),
) -> Spec(new_state) {
  case spec {
    Starting(children) -> case child_spec.start(children.state) {
      Ok(pid) -> {
        let state = child_spec.merge(children.state, pid)
        let pids = [process.make_opaque(pid), ..children.pids]
        let init = add_initialiser_layer(children.init, child_spec, pid, state)
        Starting(Children(pids: pids, state: state, init: init))
      }
    }
    Failed(fail) -> Failed(fail)
  }
}

pub fn old_worker(
  children: Spec(state),
  start start_child: fn(state) -> StartResult(msg),
  returning merge: fn(state, Pid(msg)) -> new_state,
) -> Spec(new_state) {
  children
  |> add(ChildSpec(start_child, merge))
}

pub fn unreferenced_worker(
  children: Spec(state),
  start start_child: fn(state) -> StartResult(msg),
) -> Spec(state) {
  old_worker(children, start_child, fn(x, _) { x })
}

// Testing
pub fn start_child1(x: Nil) -> StartResult(Int) {
  todo
}

pub fn start_child2(older: Pid(Int)) -> StartResult(String) {
  todo
}

pub fn start_child3(older: Pid(Int)) -> StartResult(Float) {
  todo
}

// pub fn init(spec: Spec(Nil)) {
//   spec
//   |> add(
//     worker(start_child1)
//     |> with_state(fn(_state, pid) { pid }),
//   )
//   |> add(worker(start_child2))
//   |> add(worker(start_child3))
// }
pub fn init(spec: Spec(Nil)) {
  spec
  |> old_worker(start: start_child1, returning: fn(_state, pid) { pid })
  |> unreferenced_worker(start: start_child2)
  |> unreferenced_worker(start: start_child3)
}
