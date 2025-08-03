import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/erlang/reference.{type Reference}
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision

//
// Group registry actor
//

pub type GroupRegistry

pub type GroupRegistryMessage

pub fn start_registry(
  name: Name(GroupRegistryMessage),
) -> actor.StartResult(GroupRegistry) {
  todo
}

pub fn supervised_registry(
  name: Name(GroupRegistryMessage),
) -> supervision.ChildSpecification(GroupRegistry) {
  supervision.worker(fn() { start_registry(name) })
}

@external(erlang, "gleam_otp_external", "identity")
pub fn get_registry(name: Name(GroupRegistryMessage)) -> GroupRegistry

//
// Groups
//

pub opaque type Group(message) {
  Group(tag: reference.Reference, registry: GroupRegistry)
}

pub fn new(registry: GroupRegistry) -> Group(message) {
  Group(tag: reference.new(), registry:)
}

pub fn join(group: Group(message), new_member: Pid) -> Subject(message) {
  erlang_join(group.registry, group.tag, new_member)
  subject_for_group(group, new_member)
}

pub fn leave(group: Group(message), members: List(Pid)) -> Nil {
  erlang_leave(group.registry, group.tag, members)
  Nil
}

pub fn members(group: Group(message)) -> List(Subject(message)) {
  erlang_members(group.registry, group.tag)
  |> list.map(subject_for_group(group, _))
}

//
// Helpers
//

fn subject_for_group(group: Group(message), pid: Pid) -> Subject(message) {
  let tag = reference_to_dynamic(group.tag)
  process.unsafely_create_subject(pid, tag)
}

//
// Erlang FFI
//

type DoNotLeak

@external(erlang, "pg", "join")
fn erlang_join(
  registry: GroupRegistry,
  group: Reference,
  new_members: Pid,
) -> DoNotLeak

@external(erlang, "pg", "leave")
fn erlang_leave(
  registry: GroupRegistry,
  group: Reference,
  members: List(Pid),
) -> DoNotLeak

@external(erlang, "pg", "get_members")
fn erlang_members(registry: GroupRegistry, group: Reference) -> List(Pid)

@external(erlang, "gleam_otp_external", "identity")
fn reference_to_dynamic(a: Reference) -> Dynamic
