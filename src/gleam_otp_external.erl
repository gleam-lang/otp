-module(gleam_otp_external).

% Channels
-export([open_channel/1, close_channel/1]).

% Receivers
-export([new_receiver/1, flush_receiver/1, run_receiver/2, merge_receiver/2,
         run_receiver_forever/1]).

% -export([include_process_monitor/3,
%          include_port_monitor/3, include_process_exit/3, include_system/2,
%          include_bare/2, include_all_exits/2, include_all/2, remove_timeout/1,
%          set_timeout/2
%          flush_other/2]).

%
% import Gleam records
%

-include("gen/src/gleam@otp@process_Sender.hrl").
% -include("gen/src/gleam@otp@process_Exit.hrl").
% -include("gen/src/gleam@otp@process_PortDown.hrl").
% -include("gen/src/gleam@otp@process_PortMonitor.hrl").
% -include("gen/src/gleam@otp@process_ProcessDown.hrl").
% -include("gen/src/gleam@otp@process_ProcessMonitor.hrl").
% -include("gen/src/gleam@otp@process_StatusInfo.hrl").

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

-record(receiver, {pid, channels}).

new_receiver(Ref) ->
    #receiver{pid = self(), channels = #{Ref => fun(M) -> M end}}.

close_channel(Receiver) ->
    Refs = maps:keys(Receiver#receiver.channels),
    lists:each(fun(Ref) ->
        update_channels(fun(Open) -> maps:remove(Ref, Open) end)
    end, Refs),
    nil.

assert_receiver_owner(#receiver{pid = Pid}) ->
    Pid = self().

currently_open_channels() ->
    get('$gleam_open_channels').

update_channels(Fn) ->
    case currently_open_channels() of
        undefined -> Fn(#{});
        Refs -> Fn(Refs)
    end.

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

        {Ref, Msg} when is_map_key(Ref, Receiving) ->
            transform_msg(Receiving, Ref, Msg)

        % {'DOWN', Ref, process, Pid, Reason} when is_map_key(Ref, Refs) ->
        %     transform_msg(Refs, Ref, #process_down{pid = Pid, reason = Reason});

        % {'DOWN', Ref, port, Port, Reason} when is_map_key(Ref, Refs) ->
        %     transform_msg(Refs, Ref, #port_down{port = Port, reason = Reason});

        % {'EXIT', Pid, Reason} when is_map_key(Pid, ExitPids) ->
        %     transform_msg(ExitPids, Pid, #exit{pid = Pid, reason = Reason});

        % {system, From, Request} when is_function(System) ->
        %     {ok, System(system_msg(From, Request))};

        % Msg when (not ?is_special_msg(Msg)) andalso is_function(Bare) ->
        %     {ok, Bare(Msg)};

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
            flush_receiver(Receiver, N + 1)

        % {'DOWN', Ref, _, _, _} when is_map_key(Ref, Refs) ->
        %     flush_receiver(Receiver, N + 1);

        % {'EXIT', Pid, _} when is_map_key(Pid, ExitPids) ->
        %     flush_receiver(Receiver, N + 1);

        % {system, _, _} when is_function(System) ->
        %     flush_receiver(Receiver, N + 1);

        % Msg when (not ?is_special_msg(Msg)) andalso is_function(Bare) ->
        %     flush_receiver(Receiver, N + 1);

        % _ when is_function(All) ->
        %     flush_receiver(Receiver, N + 1)
    after
        0 -> N
    end.

% include_system(Receiver, Fn) ->
%     Receiver#receiver{system = Fn}.
%
% include_bare(Receiver, Fn) ->
%     Receiver#receiver{bare = Fn}.
%
% include_all(Receiver, Fn) ->
%     Receiver#receiver{all = Fn}.
%
% include_all_exits(Receiver, Fn) ->
%     Receiver#receiver{all_exits = Fn}.
%
% flush_other(Receiver, FlushOther) ->
%     Receiver#receiver{flush_other = FlushOther}.
%
% system_msg({Pid, Ref}, Msg) ->
%     Build = fun(X) -> system_reply(Msg, Ref, X) end,
%     Kind = {system_channel, Ref, Build},
%     Channel = #channel{pid = Pid, kind = Kind},
%     {Msg, Channel}.
%
% system_reply(Msg, Ref, Reply) ->
%     Msg1 = case Msg of
%         resume -> ok;
%         suspend -> ok;
%         get_state -> Reply;
%         get_status -> process_status(Reply)
%     end,
%     {Ref, Msg1}.
%
% process_status(Status) ->
%     #status_info{mode = Mode, parent = Parent, debug_state = Debug,
%                  state = State, mod = Mod} = Status,
%     Data = [
%         get(), Mode, Parent, Debug,
%         [{header, "Status for Gleam actor " ++ pid_to_list(self())},
%          {data, [{'Status', Mode}, {'Parent', Parent}, {'State', State}]}]
%     ],
%     {status, self(), {module, Mod}, Data}.
