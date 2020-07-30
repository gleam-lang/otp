-module(gleam_otp_process_external).

% Receivers
-export([make_receiver/0, include_channel/3, include_process_monitor/3,
         include_port_monitor/3, include_system/2, include_bare/2,
         remove_timeout/1, set_timeout/2, run_receiver/1, flush_receiver/1,
         flush_other/2]).

%
% import Gleam records
%

-include("gen/src/gleam@otp@process_Channel.hrl").
-include("gen/src/gleam@otp@process_Message.hrl").
-include("gen/src/gleam@otp@process_PortDown.hrl").
-include("gen/src/gleam@otp@process_PortMonitor.hrl").
-include("gen/src/gleam@otp@process_ProcessDown.hrl").
-include("gen/src/gleam@otp@process_ProcessMonitor.hrl").
-include("gen/src/gleam@otp@process_System.hrl").

%
% Guards
%

-define(is_n_tuple(Term, N), (is_tuple(Term) andalso tuple_size(Term) =:= N)).
-define(is_record(Tag, Arity, Term),
        (?is_n_tuple(Term, Arity + 1) andalso element(1, Term) =:= Tag)).
-define(is_system_msg(Term), ?is_record(system, 2, Term)).
-define(is_monitor_msg(Term), ?is_record('DOWN', 4, Term)).
-define(is_exit_msg(Term), ?is_record('EXIT', 2, Term)).
-define(is_gen_reply(Term),
        (is_tuple(Term) andalso tuple_size(Term) =:= 2 andalso
        is_reference(element(1, Term)))).

-define(is_special_msg(Term),
        (?is_system_msg(Term) orelse ?is_monitor_msg(Term) orelse
         ?is_exit_msg(Term) orelse ?is_gen_reply(Term))).

%
% Receivers
%

-record(receiver, {system, bare, map, timeout, flush_other}).

make_receiver() ->
    #receiver{timeout = 5000, system = undefined, bare = undefined,
              flush_other = false, map = #{}}.

receiver_include(Receiver, Ref, Fn) ->
    Map = maps:put(Ref, Fn, Receiver#receiver.map),
    Receiver#receiver{map = Map}.

include_channel(Receiver, Channel, Fn) ->
    receiver_include(Receiver, Channel#channel.reference, Fn).

include_process_monitor(Receiver, Monitor, Fn) ->
    receiver_include(Receiver, Monitor#process_monitor.reference, Fn).

include_port_monitor(Receiver, Monitor, Fn) ->
    receiver_include(Receiver, Monitor#port_monitor.reference, Fn).

channel_msg(Map, Ref, Msg) ->
    Fn = maps:get(Ref, Map),
    {ok, Fn(Msg)}.

run_receiver(Receiver) ->
    #receiver{timeout = Timeout, system = System, bare = Bare, map = Map,
              flush_other = FlushOther} = Receiver,
    receive
        {Ref, Msg} when is_map_key(Ref, Map) ->
            channel_msg(Map, Ref, Msg);

        {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, Map) ->
            channel_msg(Map, Ref, #process_down{pid = Pid, reason = Reason});

        {'DOWN', Ref, port, Port, Reason} when is_map_key(Ref, Map) ->
            channel_msg(Map, Ref, #port_down{port = Port, reason = Reason});

        {system, From, Request} when is_function(System) ->
            system_msg(From, Request);

        Msg when (not ?is_special_msg(Msg)) andalso is_function(Bare) ->
            {ok, Bare(Msg)};

        _ when FlushOther ->
            % TODO: shrink timeout if time has passed
            run_receiver(Receiver)
    after
        Timeout -> {error, nil}
    end.

flush_receiver(Receiver, N) ->
    #receiver{system = System, bare = Bare, map = Map} = Receiver,
    receive
        {Ref, _} when is_map_key(Ref, Map) ->
            flush_receiver(Receiver, N + 1);

        {'DOWN', Ref, _, _, _} when is_map_key(Ref, Map) ->
            flush_receiver(Receiver, N + 1);

        {system, _, _} when is_function(System) ->
            flush_receiver(Receiver, N + 1);

        Msg when (not ?is_special_msg(Msg)) andalso is_function(Bare) ->
            flush_receiver(Receiver, N + 1)
    after
        0 -> N
    end.

set_timeout(Receiver, Timeout) ->
    Receiver#receiver{timeout = Timeout}.

remove_timeout(Receiver) ->
    Receiver#receiver{timeout = infinity}.

flush_receiver(Receiver) ->
    flush_receiver(Receiver, 0).

include_system(Receiver, Fn) ->
    Receiver#receiver{system = Fn}.

include_bare(Receiver, Fn) ->
    Receiver#receiver{bare = Fn}.

flush_other(Receiver, FlushOther) ->
    Receiver#receiver{flush_other = FlushOther}.

system_msg(Msg, {Pid, Ref}) when is_atom(Msg) ->
    {Msg, #channel{pid = Pid, reference = Ref}}.
