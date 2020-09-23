// TODO: test
// TODO: allow the IntensityTracker limit and period to be configured
import gleam/list
import gleam/pair
import gleam/result
import gleam/dynamic
import gleam/option.{None, Option, Some}
import gleam/otp/process.{Pid, Sender}
import gleam/otp/actor.{StartError}
import gleam/otp/intensity_tracker.{IntensityTracker}
import gleam/io

pub opaque type Children(argument) {
  Ready(Starter(argument))
  Failed(StartError)
}

pub opaque type ChildSpec(msg, argument_in, argument_out) {
  ChildSpec(
    start: fn(argument_in) -> Result(Sender(msg), StartError),
    update_argument: fn(argument_in, Sender(msg)) -> argument_out,
  )
}

pub opaque type Message {
  Exit(process.Exit)
}

type Instruction {
  StartAll
  StartFrom(Pid)
}

type State(a) {
  State(restarts: IntensityTracker, starter: Starter(a))
}

type Starter(argument) {
  Starter(
    argument: argument,
    starter: Option(
      fn(Instruction) ->
        Result(tuple(Starter(argument), Instruction), StartError),
    ),
  )
}

type Child(argument) {
  Child(pid: Pid, argument: argument)
}

fn start_child(
  child_spec: ChildSpec(msg, argument_in, argument_out),
  argument: argument_in,
) -> Result(Child(argument_out), StartError) {
  try channel = child_spec.start(argument)

  Ok(Child(
    pid: process.pid(channel),
    // Merge the new child's pid into the argument to produce the new argument
    // used to start any remaining children.
    argument: child_spec.update_argument(argument, channel),
  ))
}

// TODO: more sophsiticated stopping of processes. i.e. give supervisors
// more time to shut down.
fn shutdown_child(pid: Pid, _spec: ChildSpec(msg, arg_1, arg_2)) -> Nil {
  process.send_exit(pid, process.Normal)
}

fn perform_instruction_for_child(
  argument: argument_in,
  instruction: Instruction,
  child_spec: ChildSpec(msg, argument_in, argument_out),
  child: Child(argument_out),
) -> Result(tuple(Child(argument_out), Instruction), StartError) {
  let current = child.pid
  case instruction {
    // This child is older than the StartFrom target, we don't need to
    // restart it
    StartFrom(target) if target != current -> Ok(tuple(child, instruction))

    // This pid either is the cause of the problem, or we have the StartAll
    // instruction. Either way it and its younger siblings need to be restarted.
    _ -> {
      shutdown_child(current, child_spec)
      try child = start_child(child_spec, argument)
      Ok(tuple(child, StartAll))
    }
  }
}

fn add_child_to_starter(
  state: Starter(argument_in),
  child_spec: ChildSpec(msg, argument_in, argument_out),
  child: Child(argument_out),
) -> Starter(argument_out) {
  let starter = fn(instruction) {
    // Restart the older children. We use `try` to return early if the older
    // children failed to start
    try tuple(state, instruction) = case state.starter {
      Some(starter) -> starter(instruction)
      None -> Ok(tuple(state, instruction))
    }

    // Perform the instruction, restarting the child as required
    try tuple(child, instruction) =
      perform_instruction_for_child(
        state.argument,
        instruction,
        child_spec,
        child,
      )

    // Create a new starter for the next time the supervisor needs to restart
    let state = add_child_to_starter(state, child_spec, child)

    Ok(tuple(state, instruction))
  }

  Starter(starter: Some(starter), argument: child.argument)
}

fn start_and_add_child(
  state: Starter(argument_0),
  child_spec: ChildSpec(msg, argument_0, argument_1),
) -> Children(argument_1) {
  case start_child(child_spec, state.argument) {
    Ok(child) -> Ready(add_child_to_starter(state, child_spec, child))
    Error(reason) -> Failed(reason)
  }
}

pub fn add(
  children: Children(argument),
  child_spec: ChildSpec(msg, argument, new_argument),
) -> Children(new_argument) {
  case children {
    // If one of the previous children has failed then we cannot continue
    Failed(fail) -> Failed(fail)

    // If everything is OK so far then we can add the child
    Ready(state) -> start_and_add_child(state, child_spec)
  }
}

// TODO: test
// TODO: document
pub fn worker(
  start: fn(argument) -> Result(Sender(msg), StartError),
) -> ChildSpec(msg, argument, argument) {
  ChildSpec(start: start, update_argument: fn(argument, _channel) { argument })
}

// TODO: test
// TODO: document
pub fn update_argument(
  child: ChildSpec(msg, argument_a, argument_b),
  updater: fn(argument_a, Sender(msg)) -> argument_c,
) -> ChildSpec(msg, argument_a, argument_c) {
  ChildSpec(start: child.start, update_argument: updater)
}

fn init(
  start_children: fn(Children(Nil)) -> Children(a),
) -> actor.InitResult(State(a), Message) {
  // Trap exits so that we get a message when a child crashes
  let receiver =
    process.trap_exits()
    |> process.map_receiver(Exit)
    |> option.Some

  // Start any children
  let result =
    Starter(argument: Nil, starter: None)
    |> Ready
    |> start_children

  // Pass back up the result
  case result {
    Ready(starter) -> {
      let restarts = intensity_tracker.new(limit: 5, period: 1)
      let state = State(starter: starter, restarts: restarts)
      actor.Ready(state, receiver)
    }
    Failed(reason) -> {
      // TODO: refine error type
      let reason = process.Abnormal(dynamic.from(reason))
      // TODO: try to start them again
      actor.Failed(reason)
    }
  }
}

type HandleExitError {
  RestartFailed(restarts: IntensityTracker)
  TooManyRestarts
}

fn handle_exit(pid: process.Pid, state: State(a)) -> actor.Next(State(a)) {
  let outcome = {
    // If we are handling an exit then we must have some children
    assert Some(starter) = state.starter.starter

    // Check to see if there has been too many restarts in this period
    try restarts =
      state.restarts
      |> intensity_tracker.add_event
      |> result.map_error(fn(_) { TooManyRestarts })

    // Restart the exited child and any following children
    try tuple(starter, _) =
      starter(StartFrom(pid))
      |> result.map_error(fn(_) { RestartFailed(restarts) })

    Ok(State(starter: starter, restarts: restarts))
  }

  case outcome {
    Ok(state) -> actor.Continue(state)
    Error(RestartFailed(_restarts)) ->
      todo("Asynchrously enqueue another attempt to restart")
    Error(TooManyRestarts) ->
      actor.Stop(process.Abnormal(dynamic.from(TooManyRestarts)))
  }
}

fn loop(message: Message, state: State(argument)) -> actor.Next(State(argument)) {
  case message {
    Exit(process.Exit(pid: pid, ..)) -> handle_exit(pid, state)
  }
}

pub fn start(
  start_children: fn(Children(Nil)) -> Children(a),
) -> Result(Sender(Message), StartError) {
  actor.start(actor.Spec(
    init: fn() { init(start_children) },
    loop: loop,
    init_timeout: 60_000,
  ))
}
