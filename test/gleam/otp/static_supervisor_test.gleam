import gleam/erlang/process.{type Pid, type Subject}
import gleam/otp/actor
import gleam/otp/static_supervisor as sup

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
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.add(init_notifier_child(subject, "2"))
    |> sup.add(init_notifier_child(subject, "3"))
    |> sup.start_link

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

  let assert True = process.is_alive(supervisor)
  process.send_exit(supervisor)
}

pub fn rest_for_one_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    sup.new(sup.RestForOne)
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.add(init_notifier_child(subject, "2"))
    |> sup.add(init_notifier_child(subject, "3"))
    |> sup.start_link

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

  let assert True = process.is_alive(supervisor)
  process.send_exit(supervisor)
}

pub fn one_for_all_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    sup.new(sup.OneForAll)
    |> sup.add(init_notifier_child(subject, "1"))
    |> sup.add(init_notifier_child(subject, "2"))
    |> sup.add(init_notifier_child(subject, "3"))
    |> sup.start_link

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

  let assert True = process.is_alive(supervisor)
  process.send_exit(supervisor)
}
