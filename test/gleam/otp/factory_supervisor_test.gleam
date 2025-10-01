import gleam/erlang/charlist
import gleam/erlang/process.{type Pid, type Subject}
import gleam/otp/actor
import gleam/otp/factory_supervisor
import gleam/otp/supervision

// A child that sends their name back to the test process during
// initialisation so that we can tell they (re)started
fn init_notifier_child(
  subject: Subject(#(String, Pid)),
  name: String,
) -> actor.StartResult(Subject(Nil)) {
  actor.new_with_initialiser(50, fn(self_subject) {
    process.send(subject, #(name, process.self()))
    actor.initialised(name) |> actor.returning(self_subject) |> Ok
  })
  |> actor.on_message(fn(_msg, _state) { actor.stop() })
  |> actor.start
}

pub fn transient_test() {
  let subject = process.new_subject()
  let builder = factory_supervisor.worker_child(init_notifier_child(subject, _))

  // The default restart strategy is transient
  let builder_with_transient =
    factory_supervisor.restart_strategy(builder, supervision.Transient)
  assert builder == builder_with_transient

  // Start supervisor and some children
  let assert Ok(supervisor) = factory_supervisor.start(builder)

  let assert Ok(actor.Started(pid: p1, data: _)) =
    factory_supervisor.start_child(supervisor.data, "1")
  let assert Ok(actor.Started(pid: p2, data: _)) =
    factory_supervisor.start_child(supervisor.data, "2")
  let assert Ok(actor.Started(pid: p3, data: p3_subject)) =
    factory_supervisor.start_child(supervisor.data, "3")

  // Assert children have started
  assert process.receive(subject, 10) == Ok(#("1", p1))
  assert process.receive(subject, 10) == Ok(#("2", p2))
  assert process.receive(subject, 10) == Ok(#("3", p3))
  assert process.receive(subject, 10) == Error(Nil)

  // Shutdown first child and assert only it restarts
  process.kill(p1)
  let assert Ok(#("1", p1_2)) = process.receive(subject, 10)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert process.is_alive(p1_2)
  assert process.is_alive(p2)
  assert process.is_alive(p3)

  // Shutdown second child and assert only it restarts
  process.kill(p2)
  let assert Ok(#("2", p2_2)) = process.receive(subject, 10)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert process.is_alive(p1_2)
  assert process.is_alive(p2_2)
  assert process.is_alive(p3)

  // Tell the third child to exit normally. It won't restart.
  process.send(p3_subject, Nil)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert !process.is_alive(p3)
  assert process.is_alive(p1_2)
  assert process.is_alive(p2_2)

  // Children are cleaned up
  assert process.is_alive(supervisor.pid)
  process.send_exit(supervisor.pid)
  process.sleep(10)
  assert !process.is_alive(supervisor.pid)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert !process.is_alive(p1_2)
  assert !process.is_alive(p2_2)
  assert !process.is_alive(p3)
}

pub fn temporary_test() {
  let subject = process.new_subject()

  // Start supervisor and some children
  let assert Ok(supervisor) =
    factory_supervisor.worker_child(init_notifier_child(subject, _))
    |> factory_supervisor.restart_strategy(supervision.Temporary)
    // Raise intensity as we're killing multiple children in this module
    |> factory_supervisor.restart_tolerance(intensity: 10, period: 5)
    |> factory_supervisor.start

  let assert Ok(actor.Started(pid: p1, data: _)) =
    factory_supervisor.start_child(supervisor.data, "1")
  let assert Ok(actor.Started(pid: p2, data: _)) =
    factory_supervisor.start_child(supervisor.data, "2")
  let assert Ok(actor.Started(pid: p3, data: p3_subject)) =
    factory_supervisor.start_child(supervisor.data, "3")
  let assert Ok(actor.Started(pid: p4, data: _)) =
    factory_supervisor.start_child(supervisor.data, "4")

  // Assert children have started
  assert process.receive(subject, 10) == Ok(#("1", p1))
  assert process.receive(subject, 10) == Ok(#("2", p2))
  assert process.receive(subject, 10) == Ok(#("3", p3))
  assert process.receive(subject, 10) == Ok(#("4", p4))
  assert process.receive(subject, 10) == Error(Nil)

  // Shutdown first child, it is not restarted
  process.kill(p1)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert process.is_alive(p2)
  assert process.is_alive(p3)
  assert process.is_alive(p4)

  // Shutdown second child, it is not restarted
  process.kill(p2)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert process.is_alive(p3)
  assert process.is_alive(p4)

  // Tell the third child to exit normally. It won't restart.
  process.send(p3_subject, Nil)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert !process.is_alive(p3)
  assert process.is_alive(p4)

  // Children are cleaned up
  assert process.is_alive(supervisor.pid)
  process.send_exit(supervisor.pid)
  process.sleep(10)
  assert !process.is_alive(supervisor.pid)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert !process.is_alive(p3)
  assert !process.is_alive(p4)
}

pub fn permanent_test() {
  let subject = process.new_subject()

  // Start supervisor and some children
  let assert Ok(supervisor) =
    factory_supervisor.worker_child(init_notifier_child(subject, _))
    |> factory_supervisor.restart_strategy(supervision.Permanent)
    // Raise intensity as we're killing multiple children in this module
    |> factory_supervisor.restart_tolerance(intensity: 10, period: 5)
    |> factory_supervisor.start

  let assert Ok(actor.Started(pid: p1, data: _)) =
    factory_supervisor.start_child(supervisor.data, "1")
  let assert Ok(actor.Started(pid: p2, data: _)) =
    factory_supervisor.start_child(supervisor.data, "2")
  let assert Ok(actor.Started(pid: p3, data: p3_subject)) =
    factory_supervisor.start_child(supervisor.data, "3")

  // Assert children have started
  assert process.receive(subject, 10) == Ok(#("1", p1))
  assert process.receive(subject, 10) == Ok(#("2", p2))
  assert process.receive(subject, 10) == Ok(#("3", p3))
  assert process.receive(subject, 10) == Error(Nil)

  // Shutdown first child and assert only it restarts
  process.kill(p1)
  let assert Ok(#("1", p1_2)) = process.receive(subject, 10)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert process.is_alive(p1_2)
  assert process.is_alive(p2)
  assert process.is_alive(p3)

  // Shutdown second child and assert only it restarts
  process.kill(p2)
  let assert Ok(#("2", p2_2)) = process.receive(subject, 10)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert process.is_alive(p1_2)
  assert process.is_alive(p2_2)
  assert process.is_alive(p3)

  // Tell the third child to exit normally. It is restarted
  process.send(p3_subject, Nil)
  let assert Ok(#("3", p3_2)) = process.receive(subject, 10)
  assert process.receive(subject, 10) == Error(Nil)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert !process.is_alive(p3)
  assert process.is_alive(p1_2)
  assert process.is_alive(p2_2)
  assert process.is_alive(p3_2)

  // Children are cleaned up
  assert process.is_alive(supervisor.pid)
  process.send_exit(supervisor.pid)
  process.sleep(10)
  assert !process.is_alive(supervisor.pid)
  assert !process.is_alive(p1)
  assert !process.is_alive(p2)
  assert !process.is_alive(p3)
  assert !process.is_alive(p1_2)
  assert !process.is_alive(p2_2)
  assert !process.is_alive(p3_2)
}

pub fn duplicate_child_test() {
  let subject = process.new_subject()
  let assert Ok(supervisor) =
    factory_supervisor.worker_child(init_notifier_child(subject, _))
    |> factory_supervisor.start

  // Multiple children can be started with the same argument, there is no clash.
  let assert Ok(actor.Started(pid: p1, data: _)) =
    factory_supervisor.start_child(supervisor.data, "same")
  let assert Ok(actor.Started(pid: p2, data: _)) =
    factory_supervisor.start_child(supervisor.data, "same")
  let assert Ok(actor.Started(pid: p3, data: _)) =
    factory_supervisor.start_child(supervisor.data, "same")

  // Assert children have started
  assert process.receive(subject, 10) == Ok(#("same", p1))
  assert process.receive(subject, 10) == Ok(#("same", p2))
  assert process.receive(subject, 10) == Ok(#("same", p3))
  assert process.receive(subject, 10) == Error(Nil)
  process.send_exit(supervisor.pid)
}

pub fn named_test() {
  // Create a name
  let name = process.new_name("my_factory")
  let handle = factory_supervisor.get_by_name(name)

  // Create a supervisor
  let subject = process.new_subject()
  let builder =
    factory_supervisor.worker_child(init_notifier_child(subject, _))
    |> factory_supervisor.named(name)
  let assert Ok(supervisor) = factory_supervisor.start(builder)

  // Attempting to create another with the same name will fail
  assert factory_supervisor.start(builder)
    == Error(actor.InitFailed("already started"))
  // TODO: test use of the named supervisor
}
