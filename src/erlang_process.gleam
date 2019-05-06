// TODO: Tests
// TODO: README
// TODO: Inline documentation

import any
import atom

pub external type Pid;

pub external fn self() -> Pid
  = "erlang" "self";

pub external fn named(atom:Atom) -> Result(Pid, Nil)
  = "erlang_process_external" "named";

pub external fn spawn(fn() -> anything) -> Pid
  = "erlang" "spawn";

pub external fn spawn_link(fn() -> anything) -> Pid
  = "erlang" "spawn_link";

pub external fn link(Pid) -> Bool
  = "erlang_process_external" "link";

pub external fn unlink(Pid) -> Bool
  = "erlang" "unlink";

pub external fn register(Pid, atom:Atom) -> Result(atom:Atom, Nil)
  = "erlang_process_external" "register";

pub external fn unregister(atom:Atom) -> Result(atom:Atom, Nil)
  = "erlang_process_external" "unregister";

// TODO: rename when we have escaping
pub external fn receive_(Int) -> Result(any:Any, Nil)
  = "erlang_process_external" "receive_";

pub external fn send(Pid, msg) -> msg
  = "erlang_process_external" "send";

pub external fn is_alive(Pid) -> Bool
  = "erlang" "is_process_alive";

pub external fn send_exit(Pid, reason) -> Bool
  = "erlang" "exit";

pub external type MonitorRef;

enum MonitorType =
  | Process

external fn erl_monitor(MonitorType, Pid) -> MonitorRef
  = "erlang" "monitor";

pub fn monitor(pid) {
  erl_monitor(Process, pid)
}

pub external fn unmonitor(MonitorRef) -> Bool
  = "erlang" "unmonitor";
