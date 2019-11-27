-module(gleam_otp_process_external).

-export([cast/1, send_exit/2, link/1, unlink/1]).

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
