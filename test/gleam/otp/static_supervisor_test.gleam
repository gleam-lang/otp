import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/io
import gleam/otp/actor
import gleam/otp/static_supervisor as sup

@external(erlang, "supervisor", "which_children")
fn erlang_which_children(sup_ref: Pid) -> Dynamic

@external(erlang, "supervisor", "get_childspec")
fn erlang_get_childspec(sup_ref: Pid, id: Dynamic) -> Result(Dynamic, Dynamic)

fn actor_child(name name, init init, loop loop) -> sup.ChildBuilder {
  sup.worker_child(name, fn() {
    let spec = actor.Spec(init: init, init_timeout: 10, loop: loop)
    let assert Ok(subject) = actor.start_spec(spec)
    Ok(process.subject_owner(subject))
  })
}

// A child that sends their name back to the test process during
// initialisation so that we can tell they (re)started
fn init_notifier_child(
  subject: Subject(#(String, Pid)),
  name: String,
) -> sup.ChildBuilder {
  actor_child(
    name: name,
    init: fn() {
      process.send(subject, #(name, process.self()))
      actor.Ready(name, process.new_selector())
    },
    loop: fn(_msg, state) { actor.continue(state) },
  )
}

pub fn one_for_one_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    sup.new(sup.OneForOne)
    |> sup.restart_tolerance(3, 5)
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.add(init_notifier_child(subject, "2"))
    |> sup.add(init_notifier_child(subject, "3"))
    |> sup.start_link

  // Assert starting children using args does not work
  // (Should only work for a simple-one-for-one supervisor)
  let assert Error(sup.SupervisorNotSimpleOneForOne) =
    sup.start_child_with_args(supervisor, [])

  // Assert children have started
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown first child and assert only it restarts
  process.kill(p1)
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Shutdown second child and assert only it restarts
  process.kill(p2)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Start new child
  let assert Ok(_p4) =
    sup.start_child_with_builder(supervisor, init_notifier_child(subject, "4"))

  // Assert new child has started
  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown new child and assert only it restarts
  process.kill(p4)

  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)
  let assert True = process.is_alive(p4)

  let supervisor_pid = sup.get_pid(supervisor)
  let assert True = process.is_alive(supervisor_pid)
  process.send_exit(supervisor_pid)
}

pub fn rest_for_one_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    sup.new(sup.RestForOne)
    |> sup.restart_tolerance(4, 5)
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.add(init_notifier_child(subject, "2"))
    |> sup.add(init_notifier_child(subject, "3"))
    |> sup.start_link

  // Assert starting children using args does not work
  // (Should only work for a simple-one-for-one supervisor)
  let assert Error(sup.SupervisorNotSimpleOneForOne) =
    sup.start_child_with_args(supervisor, [])

  // Assert children have started
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", _p2)) = process.receive(subject, 10)
  let assert Ok(#("3", _p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown first child and assert all restart
  process.kill(p1)
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Shutdown second child and following restart
  process.kill(p2)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Start new child
  let assert Ok(_p4) =
    sup.start_child_with_builder(supervisor, init_notifier_child(subject, "4"))

  // Assert new child has started
  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown new child and assert only it restarts
  process.kill(p4)

  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)
  let assert True = process.is_alive(p4)

  // Shutdown third child and following restart
  process.kill(p3)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)
  let assert True = process.is_alive(p4)

  let supervisor_pid = sup.get_pid(supervisor)
  let assert True = process.is_alive(supervisor_pid)
  process.send_exit(supervisor_pid)
}

pub fn one_for_all_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    sup.new(sup.OneForAll)
    |> sup.restart_tolerance(4, 5)
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.add(init_notifier_child(subject, "2"))
    |> sup.add(init_notifier_child(subject, "3"))
    |> sup.start_link

  // Assert starting children using args does not work
  // (Should only work for a simple-one-for-one supervisor)
  let assert Error(sup.SupervisorNotSimpleOneForOne) =
    sup.start_child_with_args(supervisor, [])

  // Assert children have started
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", _p2)) = process.receive(subject, 10)
  let assert Ok(#("3", _p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown first child and all restart
  process.kill(p1)
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Shutdown second child and all restart
  process.kill(p2)
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Start new child
  let assert Ok(_p4) =
    sup.start_child_with_builder(supervisor, init_notifier_child(subject, "4"))

  // Assert new child has started
  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown new child and all restart
  process.kill(p4)

  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)
  let assert True = process.is_alive(p4)

  // Shutdown third child and all restart
  process.kill(p3)
  let assert Ok(#("1", p1)) = process.receive(subject, 10)
  let assert Ok(#("2", p2)) = process.receive(subject, 10)
  let assert Ok(#("3", p3)) = process.receive(subject, 10)
  let assert Ok(#("4", p4)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)
  let assert True = process.is_alive(p4)

  let supervisor_pid = sup.get_pid(supervisor)
  let assert True = process.is_alive(supervisor_pid)
  process.send_exit(supervisor_pid)
}

pub fn simple_one_for_one_test() {
  let subject = process.new_subject()

  // Ensuring a simple-one-for-one can not have multiple child specs 
  let assert Error(sup.SimpleOneForOneMultipleChildrenError) =
    sup.new(sup.SimpleOneForOne)
    |> sup.add(init_notifier_child(subject, "0"))
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.start_link

  let assert Ok(supervisor) =
    sup.new(sup.SimpleOneForOne)
    |> sup.add(init_notifier_child(subject, "0"))
    |> sup.start_link

  // Assert starting children using a child builder does not work
  let assert Error(sup.SimpleOneForOneForbidden) =
    sup.start_child_with_builder(supervisor, init_notifier_child(subject, "2"))

  // Assert no child has yet started
  let assert Error(_) = process.receive(subject, 10)

  // Count children
  let assert True =
    sup.count_children(supervisor)
    == [sup.Specs(1), sup.Active(0), sup.Supervisors(0), sup.Workers(0)]

  // Start one child
  let assert Ok(_p1) = sup.start_child_with_args(supervisor, [])

  // Assert child was started

  let assert Ok(#("0", p1)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Start other children
  let assert Ok(_p2) = sup.start_child_with_args(supervisor, [])
  let assert Ok(_p3) = sup.start_child_with_args(supervisor, [])

  // Assert other children were started

  let assert Ok(#("0", p2)) = process.receive(subject, 10)
  let assert Ok(#("0", p3)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)

  // Shutdown first child and assert only it restarts
  process.kill(p1)
  let assert Ok(#("0", p1)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Shutdown second child and assert only it restarts
  process.kill(p2)
  let assert Ok(#("0", p2)) = process.receive(subject, 10)
  let assert Error(Nil) = process.receive(subject, 10)
  let assert True = process.is_alive(p1)
  let assert True = process.is_alive(p2)
  let assert True = process.is_alive(p3)

  // Terminate third child and assert it is not restarting
  let assert Ok(_) = sup.terminate_child_with_pid(supervisor, p3)
  let assert Error(Nil) = process.receive(subject, 100)
  let assert False = process.is_alive(p3)

  let supervisor_pid = sup.get_pid(supervisor)
  let assert True = process.is_alive(supervisor_pid)
  process.send_exit(supervisor_pid)
}
