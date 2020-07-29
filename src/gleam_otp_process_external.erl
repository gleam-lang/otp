-module(gleam_otp_process_external).

% Receivers
-export([make_receiver/0, add_channel/3, add_system_channel/2,
         run_receiver/2, flush_receiver/1]).

% Channels
-export([channel_send/2]).

% Monitoring
-export([monitor_process/1, demonitor_process/1]).

-include("gen/src/gleam@otp@process_Channel.hrl").
-include("gen/src/gleam@otp@process_Message.hrl").
-include("gen/src/gleam@otp@process_ProcessDown.hrl").
-include("gen/src/gleam@otp@process_System.hrl").

%
% Receivers
%

-record(receiver, {system, map}).

make_receiver() ->
    #receiver{system = undefined, map = #{}}.

add_channel(Receiver, Channel, Fn) ->
    Ref = Channel#channel.reference,
    Map = maps:put(Ref, Fn, Receiver#receiver.map),
    Receiver#receiver{map = Map}.

channel_msg(Map, Ref, Msg) ->
    Fn = maps:get(Ref, Map),
    {ok, Fn(Msg)}.

run_receiver(Receiver, Timeout) ->
    #receiver{system = System, map = Map} = Receiver,
    receive
        {Ref, Msg} when is_map_key(Ref, Map) ->
            channel_msg(Map, Ref, Msg);

        {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, Map) ->
            channel_msg(Map, Ref, #process_down{pid = Pid, reason = Reason});

        {system, From, Request} when is_function(System) ->
            system_msg(From, Request)
    after
        Timeout -> {error, nil}
    end.

flush_receiver(Receiver, N) ->
    #receiver{system = System, map = Map} = Receiver,
    receive
        {Ref, _} when is_map_key(Ref, Map) ->
            flush_receiver(Receiver, N + 1);

        {'DOWN', Ref, process, _, _} when is_map_key(Ref, Map) ->
            flush_receiver(Receiver, N + 1);

        {system, _, _} when is_function(System) ->
            flush_receiver(Receiver, N + 1)
    after
        0 -> N
    end.

flush_receiver(Receiver) ->
    flush_receiver(Receiver, 0).

add_system_channel(Receiver, Fn) ->
    Receiver#receiver{system = Fn}.

%
% Channels
%

channel_send(#channel{pid = Pid, reference = Ref}, Msg) ->
    erlang:send(Pid, {Ref, Msg}).

monitor_process(Pid) ->
    erlang:monitor(process, Pid).

demonitor_process(Monitor) ->
    erlang:demonitor(Monitor, [flush]),
    nil.

%do_receive(Timeout) ->
%  receive
%    {system, From, Request} ->
%      #system{message = system_msg(From, Request)};
%
%    % TODO
%    % {'EXIT', Pid, Reason} ->
%    %   #exit{pid = Pid, reason = Reason};
%
%    % TODO
%    % {'DOWN', Ref, port, Port, Reason} ->
%    %   #port_down{ref = Ref, port = Port, reason = Reason};
%  end.

system_msg(Msg, {Pid, Ref}) when is_atom(Msg) ->
    {Msg, #channel{pid = Pid, reference = Ref}}.

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
