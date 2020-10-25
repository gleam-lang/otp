-module(gleam_otp_external).

% Channels
-export([close_channels/1]).

% Receivers
-export([new_receiver/1, flush_receiver/1, run_receiver/2, merge_receiver/2,
         run_receiver_forever/1, bare_message_receiver/0, trap_exits/0,
         map_receiver/2, system_receiver/0, application_stopped/0]).

% import Gleam records

-include("gen/src/gleam@otp@process_Sender.hrl").
-include("gen/src/gleam@otp@process_Exit.hrl").
-include("gen/src/gleam@otp@process_PortDown.hrl").
-include("gen/src/gleam@otp@process_ProcessDown.hrl").
-include("gen/src/gleam@otp@process_StatusInfo.hrl").

% Guards

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

% Receivers

-record(receiver, {pid, channels}).

new_receiver(Ref) ->
    open_channel(Ref),
    #receiver{pid = self(), channels = #{Ref => fun(M) -> M end}}.

trap_exits() ->
    erlang:process_flag(trap_exit, true),
    #receiver{pid = self(), channels = #{exit => fun(M) -> M end}}.

bare_message_receiver() ->
    #receiver{pid = self(), channels = #{bare => fun(M) -> M end}}.

system_receiver() ->
    #receiver{pid = self(), channels = #{system => fun(M) -> M end}}.

close_channel(Ref) when Ref =:= exit orelse Ref =:= bare ->
    ok;
close_channel({Kind, Ref}) when Kind =:= port orelse Kind =:= process ->
    erlang:demonitor(Ref);
close_channel(Ref) when is_reference(Ref) ->
    update_channels(fun(Open) -> maps:remove(Ref, Open) end).

close_channels(Receiver) ->
    Channels = maps:keys(Receiver#receiver.channels),
    lists:foreach(fun close_channel/1, Channels),
    nil.

assert_receiver_owner(#receiver{pid = Pid}) ->
    Pid = self().

currently_open_channels() ->
    case get('$gleam_open_channels') of
        undefined -> #{};
        Channels -> Channels
    end.

update_channels(Fn) ->
    put('$gleam_open_channels', Fn(currently_open_channels())).

open_channel(Ref) ->
    update_channels(fun(Refs) -> maps:put(Ref, [], Refs) end).

merge_receiver(A, B) ->
    assert_receiver_owner(A),
    assert_receiver_owner(B),
    Channels = maps:merge(A#receiver.channels, B#receiver.channels),
    A#receiver{channels = Channels}.

transform_msg(Map, Key, Msg) ->
    Fn = maps:get(Key, Map),
    {ok, Fn(Msg)}.

run_receiver_forever(Receiver) ->
    {ok, Msg} = run_receiver(Receiver, infinity),
    Msg.

run_receiver(Receiver, Timeout) ->
    assert_receiver_owner(Receiver),
    Receiving = Receiver#receiver.channels,
    OpenChannels = currently_open_channels(),
    receive
        % Message on closed channels are discarded
        {Ref, _} when not is_map_key(Ref, OpenChannels) ->
            % TODO: shrink timeout if time has passed
            run_receiver(Receiver, Timeout);

        % Message on closed channels are discarded
        {'DOWN', Ref, process, _, _} when not is_map_key({process, Ref}, OpenChannels) ->
            % TODO: shrink timeout if time has passed
            run_receiver(Receiver, Timeout);

        {'DOWN', Ref, port, _, _} when not is_map_key({port, Ref}, OpenChannels) ->
            % TODO: shrink timeout if time has passed
            run_receiver(Receiver, Timeout);

        {Ref, Msg} when is_map_key(Ref, Receiving) ->
            transform_msg(Receiving, Ref, Msg);

        {'DOWN', Ref, process, Pid, Reason} when is_map_key({process, Ref}, Receiving) ->
            transform_msg(Receiving, {process, Ref}, #process_down{pid = Pid, reason = Reason});

        {'DOWN', Ref, port, Port, Reason} when is_map_key({port, Ref}, Receiving) ->
            transform_msg(Receiving, {port, Ref}, #port_down{port = Port, reason = Reason});

        {'EXIT', Pid, Reason} when is_map_key(exit, Receiving) ->
            transform_msg(Receiving, exit, #exit{pid = Pid, reason = Reason});

        {system, From, Request} when is_map_key(system, Receiving) ->
            transform_msg(Receiving, system, system_msg(From, Request));

        Msg when (not ?is_special_msg(Msg)) andalso is_map_key(bare, Receiving) ->
            transform_msg(Receiving, bare, Msg)

        % Msg when is_function(All) ->
        %     {ok, All(Msg)};

        % _ when FlushOther ->
        %     % TODO: shrink timeout if time has passed
        %     run_receiver(Receiver)
    after
        Timeout -> {error, nil}
    end.

flush_receiver(Receiver) ->
    flush_receiver(Receiver, 0).

flush_receiver(Receiver, N) ->
    Flushing = Receiver#receiver.channels,
    OpenChannels = currently_open_channels(),
    receive
        % Messages on closed channels are _always_ discarded
        {Ref, _} when not is_map_key(Ref, OpenChannels) ->
            flush_receiver(Receiver, N); % Don't count these messages

        {Ref, _} when is_map_key(Ref, Flushing) ->
            flush_receiver(Receiver, N + 1);

        % Messages on closed channels are _always_ discarded
        {'DOWN', Ref, port, _, _} when not is_map_key({port, Ref}, OpenChannels) ->
            flush_receiver(Receiver, N); % Don't count these messages

        {'DOWN', Ref, process, _, _} when not is_map_key({process, Ref}, OpenChannels) ->
            flush_receiver(Receiver, N); % Don't count these messages

        {'DOWN', Ref, port, _, _} when is_map_key({port, Ref}, Flushing) ->
            flush_receiver(Receiver, N + 1);

        {'DOWN', Ref, process, _, _} when is_map_key({process, Ref}, Flushing) ->
            flush_receiver(Receiver, N + 1);

        {'EXIT', _, _} when is_map_key(exit, Flushing) ->
            flush_receiver(Receiver, N + 1);

        % {system, _, _} when is_function(System) ->
        %     flush_receiver(Receiver, N + 1);

        Msg when (not ?is_special_msg(Msg)) andalso is_map_key(bare, Flushing) ->
            flush_receiver(Receiver, N + 1)

        % _ when is_function(All) ->
        %     flush_receiver(Receiver, N + 1)
    after
        0 -> N
    end.

map_receiver(Receiver, F2) ->
    Wrap = fun(_, F1) ->
        fun(X) -> F2(F1(X)) end
    end,
    Channels = maps:map(Wrap, Receiver#receiver.channels),
    Receiver#receiver{channels = Channels}.

system_msg({Pid, Ref}, Tag) ->
    Prepare = fun(X) -> system_reply(Tag, Ref, X) end,
    Sender = #sender{pid = Pid, reference = Ref, prepare = {some, Prepare}},
    {Tag, Sender}.

system_reply(Tag, Ref, Reply) ->
    Msg = case Tag of
        resume -> ok;
        suspend -> ok;
        get_state -> Reply;
        get_status -> process_status(Reply)
    end,
    {Ref, Msg}.

process_status(Status) ->
    #status_info{mode = Mode, parent = Parent, debug_state = Debug,
                 state = State, mod = Mod} = Status,
    Data = [
        get(), Mode, Parent, Debug,
        [{header, "Status for Gleam process " ++ pid_to_list(self())},
         {data, [{'Status', Mode}, {'Parent', Parent}, {'State', State}]}]
    ],
    {status, self(), {module, Mod}, Data}.

application_stopped() ->
    ok.
