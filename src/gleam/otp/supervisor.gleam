// TODO: test
// TODO: specify amount of time permitted for shut-down
import gleam/result
import gleam/dynamic
import gleam/option.{None, Option, Some}
import gleam/otp/process.{Pid, Sender}
import gleam/otp/actor.{StartError}
import gleam/otp/intensity_tracker.{IntensityTracker}
import gleam/otp/node.{Node}

/// This data structure holds all the values required by the `start_spec`
/// function in order to create an supervisor.
///
/// If you do not need to configure the behaviour of your supervisor consider
/// using the `start` function.
///
pub type Spec(argument, return) {
  Spec(
    argument: argument,
    max_frequency: Int,
    frequency_period: Int,
    init: fn(Children(argument)) -> Children(return),
  )
}

/// This type represents the starting children of a supervisor within the
/// `init` function.
///
pub opaque type Children(argument) {
  Ready(Starter(argument))
  Failed(ChildStartError)
}

/// This type contains all the information required to start a new child and
/// add it to the `Children`.
///
/// This is typically created with the `worker` function.
///
pub opaque type ChildSpec(msg, argment, returning) {
  ChildSpec(
    start: fn(argment) -> Result(Sender(msg), StartError),
    returning: fn(argment, Sender(msg)) -> returning,
  )
}

type ChildStartError {
  ChildStartError(previous_pid: Option(Pid), error: StartError)
}

pub opaque type Message {
  Exit(process.Exit)
  RetryRestart(process.Pid)
}

type Instruction {
  StartAll
  StartFrom(Pid)
}

type State(a) {
  State(
    restarts: IntensityTracker,
    starter: Starter(a),
    retry_restart_channel: process.Sender(process.Pid),
  )
}

type Starter(argument) {
  Starter(
    argument: argument,
    exec: Option(
      fn(Instruction) ->
        Result(#(Starter(argument), Instruction), ChildStartError),
    ),
  )
}

type Child(argument) {
  Child(pid: Pid, argument: argument)
}

fn start_child(
  child_spec: ChildSpec(msg, argument_in, argument_out),
  argument: argument_in,
) -> Result(Child(argument_out), ChildStartError) {
  try channel =
    child_spec.start(argument)
    |> result.map_error(ChildStartError(None, _))

  Ok(Child(
    pid: process.pid(channel),
    // Merge the new child's pid into the argument to produce the new argument
    // used to start any remaining children.
    argument: child_spec.returning(argument, channel),
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
) -> Result(#(Child(argument_out), Instruction), ChildStartError) {
  let current = child.pid
  case instruction {
    // This child is older than the StartFrom target, we don't need to
    // restart it
    StartFrom(target) if target != current -> Ok(#(child, instruction))

    // This pid either is the cause of the problem, or we have the StartAll
    // instruction. Either way it and its younger siblings need to be restarted.
    _ -> {
      shutdown_child(current, child_spec)
      try child = start_child(child_spec, argument)
      Ok(#(child, StartAll))
    }
  }
}

fn add_child_to_starter(
  starter: Starter(argument_in),
  child_spec: ChildSpec(msg, argument_in, argument_out),
  child: Child(argument_out),
) -> Starter(argument_out) {
  let starter = fn(instruction) {
    // Restart the older children. We use `try` to return early if the older
    // children failed to start
    try #(starter, instruction) = case starter.exec {
      Some(start) -> start(instruction)
      None -> Ok(#(starter, instruction))
    }

    // Perform the instruction, restarting the child as required
    try #(child, instruction) =
      perform_instruction_for_child(
        starter.argument,
        instruction,
        child_spec,
        child,
      )

    // Create a new starter for the next time the supervisor needs to restart
    let starter = add_child_to_starter(starter, child_spec, child)

    Ok(#(starter, instruction))
  }

  Starter(exec: Some(starter), argument: child.argument)
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

/// Add a child to the collection of children of the supervisor
///
/// This function starts the child from the child spec.
///
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
// TODO: unlimitd shut down duration
/// Prepare a new supervisor type child.
///
/// If you wish to prepare a new non-supervisor type child see the `worker`
/// function.
///
/// If you wish to change the type of the argument for later children see the
/// `returning` function.
///
/// Note: Gleam supervisors do not yet support different shutdown periods per
/// child so this function is currently identical in behaviour to `worker`. It is
/// recommended to use this function for supervisor children nevertheless so the
/// correct shut down behaviour is used in later releases of this library.
///
pub fn supervisor(
  start: fn(argument) -> Result(Sender(msg), StartError),
) -> ChildSpec(msg, argument, argument) {
  ChildSpec(start: start, returning: fn(argument, _channel) { argument })
}

/// Prepare a new worker type child.
///
/// If you wish to prepare a new supervisor type child see the `supervisor`
/// function.
///
/// If you wish to change the type of the argument for later children see the
/// `returning` function.
///
pub fn worker(
  start: fn(argument) -> Result(Sender(msg), StartError),
) -> ChildSpec(msg, argument, argument) {
  ChildSpec(start: start, returning: fn(argument, _channel) { argument })
}

// TODO: test
/// As each child is added to a supervisors children a new argument is prepared
/// with which to start the next child. By default argument is the same as the
/// previous argument, but this function can be used to change it to something
/// else by passing a function that takes the previous argument and the sender
/// of the previous child.
///
pub fn returning(
  child: ChildSpec(msg, argument_a, argument_b),
  updater: fn(argument_a, Sender(msg)) -> argument_c,
) -> ChildSpec(msg, argument_a, argument_c) {
  ChildSpec(start: child.start, returning: updater)
}

fn init(
  spec: Spec(argument, return),
) -> actor.InitResult(State(return), Message) {
  // Create a channel so that we can asynchronously retry restarting when we
  // fail to bring an exited child
  let #(retry_sender, retry_receiver) = process.new_channel()
  let retry_receiver = process.map_receiver(retry_receiver, RetryRestart)

  // Trap exits so that we get a message when a child crashes
  let exit_receiver =
    process.trap_exits()
    |> process.map_receiver(Exit)

  // Combine receivers
  let receiver =
    exit_receiver
    |> process.merge_receiver(retry_receiver)

  // Start any children
  let result =
    Starter(argument: spec.argument, exec: None)
    |> Ready
    |> spec.init

  // Pass back up the result
  case result {
    Ready(starter) -> {
      let restarts =
        intensity_tracker.new(
          limit: spec.max_frequency,
          period: spec.frequency_period,
        )
      let state =
        State(
          starter: starter,
          restarts: restarts,
          retry_restart_channel: retry_sender,
        )
      actor.Ready(state, Some(receiver))
    }
    Failed(reason) -> {
      // TODO: refine error type
      let reason = process.Abnormal(dynamic.from(reason))
      actor.Failed(reason)
    }
  }
}

type HandleExitError {
  RestartFailed(pid: Pid, restarts: IntensityTracker)
  TooManyRestarts
}

fn handle_exit(pid: process.Pid, state: State(a)) -> actor.Next(State(a)) {
  let outcome = {
    // If we are handling an exit then we must have some children
    assert Some(start) = state.starter.exec

    // Check to see if there has been too many restarts in this period
    try restarts =
      state.restarts
      |> intensity_tracker.add_event
      |> result.map_error(fn(_) { TooManyRestarts })

    // Restart the exited child and any following children
    try #(starter, _) =
      start(StartFrom(pid))
      |> result.map_error(fn(e: ChildStartError) {
        RestartFailed(option.unwrap(e.previous_pid, pid), restarts)
      })

    Ok(State(..state, starter: starter, restarts: restarts))
  }

  case outcome {
    Ok(state) -> actor.Continue(state)
    Error(RestartFailed(failed_child, restarts)) -> {
      // Asynchronously enqueue the restarting of this child again as we were
      // unable to restart them this time. We do this asynchronously as we want
      // to have a chance to handle any system messages that have come in.
      process.send(state.retry_restart_channel, failed_child)
      let state = State(..state, restarts: restarts)
      actor.Continue(state)
    }
    Error(TooManyRestarts) ->
      actor.Stop(process.Abnormal(dynamic.from(TooManyRestarts)))
  }
}

fn loop(message: Message, state: State(argument)) -> actor.Next(State(argument)) {
  case message {
    Exit(process.Exit(pid: pid, ..)) -> handle_exit(pid, state)
    RetryRestart(pid) -> handle_exit(pid, state)
  }
}

/// Start a supervisor from a given specification.
///
pub fn start_spec(spec: Spec(a, b)) -> Result(Sender(Message), StartError) {
  actor.start_spec(actor.Spec(
    init: fn() { init(spec) },
    loop: loop,
    init_timeout: 60_000,
  ))
}

/// Start a supervisor from a given `init` function.
///
/// If you wish to have more control over the configuration of the supervisor
/// see the `start_spec` function.
///
pub fn start(
  init: fn(Children(Nil)) -> Children(a),
) -> Result(Sender(Message), StartError) {
  start_spec(Spec(
    init: init,
    argument: Nil,
    max_frequency: 5,
    frequency_period: 1,
  ))
}

/// A type used to describe the situation in which an Erlang based application
/// is starting.
///
/// For more information see the [Erlang distributed application
/// documentation][1] and the Learn Your Some Erlang chapter on [distributed
/// applications][2].
///
/// [1]: https://erlang.org/doc/design_principles/distributed_applications.html
/// [2]: https://learnyousomeerlang.com/distributed-otp-applications
///
pub type ApplicationStartMode {
  Normal
  Takeover(Node)
  Failover(Node)
}

pub external type ApplicationStop

pub external fn application_stopped() -> ApplicationStop =
  "gleam_otp_external" "application_stopped"

/// The result of starting a Gleam actor.
///
/// This type is compatible with Gleam supervisors. If you wish to convert it
/// to a type compatible with Erlang supervisors see the `ErlangStartResult`
/// type and `erlang_start_result` function.
///
pub type StartResult(msg) =
  actor.StartResult(msg)

/// An Erlang supervisor compatible process start result.
///
pub type ErlangStartResult =
  actor.ErlangStartResult

/// Convert a Gleam actor start result into an Erlang supervisor compatible
/// process start result.
///
pub fn to_erlang_start_result(res: StartResult(msg)) -> ErlangStartResult {
  actor.to_erlang_start_result(res)
}

/// Processes written in Erlang or other BEAM languages use bare processes rather
/// than channels, so the value returned from their process start functions are
/// not compatible with Gleam supervisors. This function can be used to wrap the
/// return value so it can be used with a Gleam supervisor.
///
pub fn from_erlang_start_result(
  start: Result(Pid, error),
) -> StartResult(anything) {
  actor.from_erlang_start_result(start)
}

/// Processes written in Erlang or other BEAM languages use bare processes rather
/// than channels, so the value returned from their process start functions are
/// not compatible with Gleam supervisors. This function can be used to wrap the
/// start function so it can be used with a Gleam supervisor.
///
pub fn wrap_erlang_starter(
  start: fn() -> Result(Pid, error),
) -> fn() -> StartResult(anything) {
  actor.wrap_erlang_starter(start)
}
