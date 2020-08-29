// TODO: test
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/otp/process.{Channel, Pid}
import gleam/otp/actor.{StartError}

pub opaque type Children(argument) {
  Starting(argument: argument, starter: Starter(argument))
  Failed(StartError)
}

pub opaque type ChildSpec(msg, argument_in, argument_out) {
  ChildSpec(
    start: fn(argument_in) -> Result(Channel(msg), StartError),
    update_argument: fn(argument_in, Channel(msg)) -> argument_out,
  )
}

type Instruction {
  StartAll
  StartFrom(Pid)
}

type Next(a, b) {
  Done(a)
  More(b)
}

type StarterState(argument) {
  StarterState(
    argument: argument,
    instruction: Instruction,
    starter: Next(
      argument,
      fn(Instruction) -> Result(StarterState(argument), StartError),
    ),
  )
}

type Starter(argument) =
  fn(Instruction) -> Result(StarterState(argument), StartError)

pub fn new_children(argument: argument) -> Children(argument) {
  Starting(
    argument: argument,
    starter: fn(instruction) {
      Ok(StarterState(
        argument: argument,
        instruction: instruction,
        starter: Done(argument),
      ))
    },
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
  starter: Next(argument_in, Starter(argument_in)),
  child_spec: ChildSpec(msg, argument_in, argument_out),
  child: Child(argument_out),
) -> Starter(argument_out) {
  fn(instruction) {
    // Restart the older children. We use `try` to return early if the older
    // children failed to start
    try state = case starter {
      // There are more children to start
      More(starter) -> starter(instruction)
      // We have reached the end of the children to start, create a new state
      Done(argument) ->
        Ok(StarterState(
          argument: argument,
          instruction: instruction,
          starter: Done(argument),
        ))
    }
    let argument = state.argument

    // Perform the instruction, starting or restarting the child as required
    try tuple(child, instruction) =
      perform_instruction_for_child(argument, instruction, child_spec, child)

    // Create a new starter for the next time the supervisor needs to restart
    let starter = add_child_to_starter(state.starter, child_spec, child)

    Ok(StarterState(
      argument: child.argument,
      instruction: instruction,
      starter: More(starter),
    ))
  }
}

fn start_and_add_child(
  argument: argument_0,
  starter: Next(argument_0, Starter(argument_0)),
  child_spec: ChildSpec(msg, argument_0, argument_1),
) -> Children(argument_1) {
  case start_child(child_spec, argument) {
    Ok(child) -> {
      let starter = add_child_to_starter(starter, child_spec, child)
      Starting(argument: child.argument, starter: starter)
    }

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
    Starting(argument: argument, starter: starter) ->
      start_and_add_child(argument, More(starter), child_spec)
  }
}

// TODO: test
// TODO: document
pub fn worker_child(
  start: fn(argument) -> Result(Channel(msg), StartError),
) -> ChildSpec(msg, argument, argument) {
  ChildSpec(start: start, update_argument: fn(argument, _channel) { argument })
}

// TODO: test
// TODO: document
pub fn update_argument(
  child: ChildSpec(msg, argument_a, argument_b),
  updater: fn(argument_a, Channel(msg)) -> argument_c,
) -> ChildSpec(msg, argument_a, argument_c) {
  ChildSpec(start: child.start, update_argument: updater)
}
