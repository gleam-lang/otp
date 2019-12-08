-module(gleam_otp_process_external).

-export([cast/1, send_exit/2, link/1, unlink/1, own_pid/1, own_pid/0,
         do_spawn/1, do_spawn_link/1, do_receive/2, do_receive/1]).

-define(exit_msg_constructor, gleam_otp_process_exit_msg_constructor).

cast(X) -> X.

send_exit(Pid, Reason) ->
  exit(Pid, Reason),
  nil.

link(Pid) ->
  try
    erlang:link(Pid),
    linked
  catch
    error:noproc -> process_not_found
  end.

unlink(Pid) ->
  erlang:unlink(Pid),
  receive
    {'EXIT', Pid, _} -> nil
  after
    0 -> nil
  end.

own_pid() ->
  self().

own_pid(_) ->
  self().

do_spawn(Fn) ->
  spawn(fun() -> Fn(self) end).

do_spawn_link(Fn) ->
  spawn_link(fun() -> Fn(self) end).

do_receive(_Self, Timeout) ->
  do_receive(Timeout).

do_receive(Timeout) ->
  ExitMsgConstructor = get(?exit_msg_constructor),
  receive
    {'EXIT', From, Reason} when is_pid(From), is_function(ExitMsgConstructor) ->
      {ok, ExitMsgConstructor(From, Reason)};

    OtherMsg ->
      {ok, OtherMsg}
  after
    Timeout -> {error, nil}
  end.
