-module(erlang_process_external).

-export([send/2, named/1, receive_/1, link/1, register/2, unregister/1]).

register(Pid, Name) ->
  try erlang:register(Name, Pid) of
    _ -> {ok, Name}
  catch
    error:badarg -> {error, nil}
  end.

unregister(Name) ->
  try erlang:unregister(Name) of
    _ -> {ok, Name}
  catch
    error:badarg -> {error, nil}
  end.

link(Pid) ->
  try erlang:link(Pid) catch
    error:noproc -> false
  end.

named(Name) ->
  case whereis(Name) of
    Pid when is_pid(Pid) -> {ok, Pid};
    _ -> {error, nil}
  end.

receive_(Timeout) ->
  receive
    Msg -> {ok, Msg}
  after
    Timeout -> {error, nil}
  end.

send(Pid, Msg) ->
  Pid ! Msg.
