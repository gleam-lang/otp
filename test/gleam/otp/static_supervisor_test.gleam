import gleam/erlang/process.{type Pid, type Subject}
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision

fn actor_child(
  name name,
  init init,
  loop loop,
) -> supervision.ChildSpecification(Nil) {
  supervision.worker(name, fn() {
    actor.new_with_initialiser(50, init)
    |> actor.on_message(loop)
    |> actor.start
  })
}

// A child that sends their name back to the test process during
// initialisation so that we can tell they (re)started
fn init_notifier_child(
  subject: Subject(#(String, Pid)),
  name: String,
) -> supervision.ChildSpecification(Nil) {
  actor_child(
    name: name,
    init: fn(_) {
      process.send(subject, #(name, process.self()))
      Ok(actor.initialised(name))
    },
    loop: fn(_msg, state) { actor.continue(state) },
  )
}

pub fn one_for_one_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(init_notifier_child(subject, "1"))
    |> static_supervisor.add(init_notifier_child(subject, "2"))
    |> static_supervisor.add(init_notifier_child(subject, "3"))
    |> static_supervisor.start

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

  let assert True = process.is_alive(supervisor.pid)
  process.send_exit(supervisor.pid)
}

pub fn rest_for_one_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    static_supervisor.new(static_supervisor.RestForOne)
    |> static_supervisor.add(init_notifier_child(subject, "1"))
    |> static_supervisor.add(init_notifier_child(subject, "2"))
    |> static_supervisor.add(init_notifier_child(subject, "3"))
    |> static_supervisor.start

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

  let assert True = process.is_alive(supervisor.pid)
  process.send_exit(supervisor.pid)
}

pub fn one_for_all_test() {
  let subject = process.new_subject()

  let assert Ok(supervisor) =
    static_supervisor.new(static_supervisor.OneForAll)
    |> static_supervisor.add(init_notifier_child(subject, "1"))
    |> static_supervisor.add(init_notifier_child(subject, "2"))
    |> static_supervisor.add(init_notifier_child(subject, "3"))
    |> static_supervisor.start

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

  let assert True = process.is_alive(supervisor.pid)
  process.send_exit(supervisor.pid)
}
