import gleam/dynamic.{Dynamic}
import gleam/should
import gleam/list
import gleam/otp/basic_supervisor
import gleam/otp/process.{Pid}

external fn unsafe_get_children(
  Pid(a),
) -> List(tuple(Dynamic, Pid(b), Dynamic, Dynamic)) =
  "supervisor" "which_children"

pub fn start_link_test() {
  let child = fn(id) {
    let child_sup_spec = basic_supervisor.Spec(
      strategy: basic_supervisor.OneForOne,
      intensity: 1,
      period: 5,
      children: [],
    )
    basic_supervisor.SupervisorSpec(
      id: id,
      start: fn() { basic_supervisor.start_link(child_sup_spec) },
    )
  }

  assert Ok(
    sup,
  ) = basic_supervisor.start_link(
    basic_supervisor.Spec(
      strategy: basic_supervisor.OneForOne,
      intensity: 1,
      period: 5,
      children: [child("one"), child("two"), child("three")],
    ),
  )

  let children = unsafe_get_children(sup)

  should.equal(list.length(children), 3)
}
