-module(gleam_otp_process_external).

-export([cast/1, send_exit/2, link/1, unlink/1, own_pid/1, own_pid/0,
         spawn/2, receive_/2, receive_/1]).

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

own_pid()  -> self().
own_pid(_) -> self().

-record(self, {trap_exit_msg_constructor = nil}).

apply_flag({K, V}, Self) when K =:= min_heap_size; K =:= min_bin_vheap_size; K =:= message_queue_data; K =:= save_calls ->
  process_flag(K, V),
  Self;
apply_flag({max_heap_size, Size, Kill}, Self) ->
  process_flag(max_heap_size, #{size => Size, kill => Kill}),
  Self;
apply_flag(K = sensitive, Self) ->
  process_flag(K, true),
  Self;
apply_flag({trap_exit, F}, Self) ->
  Self#self{trap_exit_msg_constructor = F};
apply_flag(link, Self) ->
  Self.

spawn(Fn, Flags) ->
  Spawner = case lists:member(link, Flags) of
    true  -> fun spawn_link/1;
    false -> fun spawn/1
  end,
  Spawner(fun() ->
    Fn(lists:foldl(fun apply_flag/2, #self{}, Flags))
  end).

receive_(Self, Timeout) ->
  Exit = Self#self.trap_exit_msg_constructor,
  receive
    {'EXIT', From, Reason} when is_pid(From), is_function(Exit) ->
      {ok, Exit(From, Reason)};

    OtherMsg ->
      {ok, OtherMsg}
  after
    Timeout -> {error, nil}
  end.

receive_(Timeout) ->
  receive
    Msg -> Msg
  after
    Timeout -> {error, nil}
  end.
