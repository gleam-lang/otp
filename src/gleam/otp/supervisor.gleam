// TODO: test
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/otp/process.{ExitReason, OpaquePid, Pid, StartResult}

// API
pub opaque type Spec(state) {
  Starting(pids: List(OpaquePid), state: state, starter: Starter(Nil, state))
  Failed(ExitReason)
}

pub opaque type ChildSpec(msg, state_in, state_out) {
  ChildSpec(
    start: fn(state_in) -> StartResult(msg),
    update_state: fn(state_in, Pid(msg)) -> state_out,
  )
}

type StarterAcc(state) {
  StarterAcc(state: state, pids: List(OpaquePid))
}

type StarterResult(state) =
  Result(StarterAcc(state), ExitReason)

// A starter is a function that can be called to start or restart the children
// of the supervisor.
//
// The function takes 2 arguments:
// - The state computed by previous layers
// - The pid (if present) that has a problem and thus needs to be restarted
//   (along with any younger children).
//
type Starter(state_in, state_out) =
  fn(state_in, Option(OpaquePid)) -> StarterResult(state_out)

type Child(msg, state) {
  Child(pid: Pid(msg), state: state)
}

fn start_starter(
  child_spec: ChildSpec(msg, state_in, state_out),
  acc: StarterAcc(state_in),
  problem_pid: Option(OpaquePid),
) -> StarterResult(state_out) {
  try new_pid = child_spec.start(acc.state)
  let opaque_pid = process.make_opaque(new_pid)

  // If we have found the problem_pid then we don't need to keep looking, so
  // set the `problem_pid` to None while running the remaining Starter layers.
  let problem_pid = case problem_pid {
    Some(pid) if opaque_pid != pid -> problem_pid
    _ -> None
  }

  // Merge the new child's pid into the state to be potentially used by its
  // younger siblings.
  let new_state = child_spec.update_state(acc.state, new_pid)

  // Add this new child to the list of children
  let pids = [process.make_opaque(new_pid), ..acc.pids]
  Ok(StarterAcc(state: new_state, pids: pids))
}

fn run_starter(
  acc: StarterAcc(state_1),
  child_spec: ChildSpec(msg, state_1, state_2),
  problem_pid: Option(OpaquePid),
  child: Child(msg, state_2),
) -> StarterResult(state_2) {
  let opaque_pid = process.make_opaque(child.pid)
  case problem_pid {
    // This Pid is still alive and we have not found the problem pid yet.
    // In this case we don't need to restart it, we keep the existing pid
    // and continue to the next.
    Some(problem) if problem != opaque_pid -> {
      let pids = [opaque_pid, ..acc.pids]
      Ok(StarterAcc(state: child.state, pids: pids))
    }

    // This pid either is the cause of the problem, or we don't have problem
    // pid to compare with. In either case it must be restarted.
    _ -> {
      process.send_exit(child.pid, process.Normal)
      start_starter(child_spec, acc, problem_pid)
    }
  }
}

fn add_starter_layer(
  starter: Starter(state_0, state_1),
  child_spec: ChildSpec(msg, state_1, state_2),
  child: Child(msg, state_2),
) -> Starter(state_0, state_2) {
  fn(state, problem_pid) {
    case starter(state, problem_pid) {
      // Older siblings failed, exit early
      Error(fail) -> Error(fail)

      // Older siblings ok, initialise the new process
      Ok(acc) -> run_starter(acc, child_spec, problem_pid, child)
    }
  }
}

pub fn begin() -> Spec(Nil) {
  Starting(
    state: Nil,
    pids: [],
    starter: fn(state, _) { Ok(StarterAcc(state, [])) },
  )
}

fn start_and_add_child(
  pids: List(OpaquePid),
  state: state,
  starter: Starter(Nil, state),
  child_spec: ChildSpec(msg, state, new_state),
) -> Spec(new_state) {
  case child_spec.start(state) {
    Ok(pid) -> {
      let pids = [process.make_opaque(pid), ..pids]
      let state = child_spec.update_state(state, pid)
      let child = Child(pid: pid, state: state)
      let starter = add_starter_layer(starter, child_spec, child)
      Starting(pids: pids, state: state, starter: starter)
    }

    Error(reason) -> Failed(reason)
  }
}

pub fn add(
  spec: Spec(state),
  child_spec: ChildSpec(msg, state, new_state),
) -> Spec(new_state) {
  case spec {
    // If one of the previous children has failed then we cannot continue
    Failed(fail) -> Failed(fail)

    // If everything is OK so far then we can add the child
    Starting(
      state: state,
      starter: starter,
      pids: pids,
    ) -> start_and_add_child(pids, state, starter, child_spec)
  }
}

// TODO: document
pub fn worker(
  start: fn(state) -> StartResult(msg),
) -> ChildSpec(msg, state, state) {
  ChildSpec(start: start, update_state: fn(state, _pid) { state })
}

// TODO: document
pub fn returning_state(
  child: ChildSpec(msg, state_a, state_b),
  update_state: fn(state_a, Pid(msg)) -> state_c,
) -> ChildSpec(msg, state_a, state_c) {
  ChildSpec(start: child.start, update_state: update_state)
}

// Testing
pub fn start_child1(x: Nil) -> StartResult(Int) {
  todo
}

pub fn start_child2(_older: Pid(Int)) -> StartResult(String) {
  todo
}

pub fn start_child3(_older: Pid(Int)) -> StartResult(Float) {
  todo
}

pub fn init(spec) {
  spec
  |> add(
    worker(start_child1)
    |> returning_state(fn(_state, pid) { pid }),
  )
  |> add(worker(start_child2))
  |> add(worker(start_child3))
}
