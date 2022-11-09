-module(gleam_otp_test_external).

-export([flush/0, get_message_queue_length/1]).

flush() ->
    receive _ -> flush()
    after 0 -> nil
    end.

get_message_queue_length(Pid) ->
  {_, Length} = process_info(Pid, message_queue_len),
  Length.
