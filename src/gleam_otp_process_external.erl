-module(gleam_otp_process_external).

-export([receive_system_message_forever/0]).

% Channels
-export([channel_send/2, channels_receive/2, flush_channel_messages/1]).

-include("gen/src/gleam@otp@process_Channel.hrl").
-include("gen/src/gleam@otp@process_Message.hrl").
-include("gen/src/gleam@otp@process_System.hrl").

-define(is_n_tuple(Term, N), is_tuple(Term) andalso tuple_size(Term) =:= N).
-define(is_record(Tag, Arity, Term),
        is_n_tuple(Term, Arity + 1) andalso element(1, Term) =:= Tag).
-define(is_system_msg(Term), ?is_record(system, 2, Term)).
-define(is_monitor_msg(Term), ?is_record('DOWN', 4, Term)).
-define(is_exit_msg(Term), ?is_record('EXIT', 2, Term)).
-define(is_gen_reply(Term),
        is_tuple(Term) andalso tuple_size(Term) =:= 2 andalso
        is_reference(element(1, Term))).

-define(is_special_msg(Term),
        (?is_system_msg(Term) orelse ?is_monitor_msg(Term) orelse
         ?is_exit_msg(Term) orelse ?is_gen_reply(Term))).

channel_send(#channel{pid = Pid, reference = Ref}, Msg) ->
    erlang:send(Pid, {Ref, Msg}).

channel_map(Channels) ->
    Insert = fun(C, Refs) -> maps:put(C#channel.reference, C, Refs) end,
    lists:foldl(Insert, #{}, Channels).

channels_receive(Channels, Timeout) ->
    Map = channel_map(Channels),
    receive
        {Ref, Msg} when is_map_key(Ref, Map) ->
            {ok, {maps:get(Ref, Map), Msg}}
    after
        Timeout -> {error, nil}
    end.

flush_channel_messages(Channels, N) ->
    receive
        {Ref, _} when is_map_key(Ref, Channels) ->
            flush_channel_messages(Channels, N + 1)
    after
        0 -> N
    end.

flush_channel_messages(Channels) ->
    flush_channel_messages(channel_map(Channels), 0).

receive_system_message_forever() ->
    receive
        {system, From, Request} -> normalise_system_msg(From, Request)
    end.

%do_receive(Timeout) ->
%  receive
%    {system, From, Request} ->
%      #system{message = normalise_system_msg(From, Request)};
%
%    % TODO
%    % {'EXIT', Pid, Reason} ->
%    %   #exit{pid = Pid, reason = Reason};
%
%    % TODO
%    % {'DOWN', Ref, process, Pid, Reason} ->
%    %   #process_down{ref = Ref, pid = Pid, reason = Reason};
%
%    % TODO
%    % {'DOWN', Ref, port, Port, Reason} ->
%    %   #port_down{ref = Ref, port = Port, reason = Reason};
%
%    Msg ->
%      #message{message = Msg}
%  after
%    Timeout -> {error, nil}
%  end.

normalise_system_msg(From, Msg) when is_atom(Msg) ->
    {Msg, gen_from_to_channel(From)}.

% This function is implemented in Erlang as it requires selective receives.
% It is based off of gen:do_call/4.
% sync_send(Process, MakeMsg, Timeout) ->
%   {RequestRef, Replier} = new_from(Process),
%   erlang:send(Process, MakeMsg(Replier), [noconnect]),
%   receive
%     {RequestRef, Reply} ->
%       erlang:demonitor(RequestRef, [flush]),
%       Reply;
%
%     {'DOWN', RequestRef, _, _, noconnection} ->
%       Node = node(Process),
%       exit({nodedown, Node});
%
%     {'DOWN', RequestRef, _, _, Reason} ->
%       exit(Reason)
%   after
%     Timeout ->
%       erlang:demonitor(RequestRef, [flush]),
%       exit(timeout)
%   end.

gen_from_to_channel({Pid, Ref}) ->
  #channel{pid = Pid, reference = Ref}.
