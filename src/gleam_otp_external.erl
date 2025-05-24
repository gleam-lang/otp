-module(gleam_otp_external).

-export([
    application_stopped/0, convert_system_message/1, identity/1,
    make_timeout/1, convert_erlang_start_error/1
]).

identity(X) -> X.

make_timeout(X) when X < 0 -> infinity;
make_timeout(X) -> X.

% TODO: support other system messages
%   {replace_state, StateFn}
%   {change_code, Mod, Vsn, Extra}
%   {terminate, Reason}
%   {debug, {log, Flag}}
%   {debug, {trace, Flag}}
%   {debug, {log_to_file, FileName}}
%   {debug, {statistics, Flag}}
%   {debug, no_debug}
%   {debug, {install, {Func, FuncState}}}
%   {debug, {install, {FuncId, Func, FuncState}}}
%   {debug, {remove, FuncOrId}}
convert_system_message({system, {From, Ref}, Request}) when is_pid(From) ->
    Reply = fun(Msg) ->
        case Ref of 
            [alias|Alias] = Tag when is_reference(Alias) ->
                erlang:send(Alias, {Tag, Msg});
            [[alias|Alias] | _] = Tag when is_reference(Alias) ->
                erlang:send(Alias, {Tag, Msg});
            _ ->
                erlang:send(From, {Ref, Msg})
        end,
        nil
    end,
    System = fun(Callback) ->
        {system, {Request, Callback}}
    end,
    case Request of
        get_status -> System(fun(Status) -> Reply(process_status(Status)) end);
        get_state -> System(fun(State) -> Reply({ok, State}) end);
        suspend -> System(fun() -> Reply(ok) end);
        resume -> System(fun() -> Reply(ok) end);
        Other -> {unexpected, Other}
    end.

process_status({status_info, Module, Parent, Mode, DebugState, State}) ->
     Data = [
         get(), Mode, Parent, DebugState,
         [{header, "Status for Gleam process " ++ pid_to_list(self())},
           {data, [
             {"Gleam behaviour", Module},
             {"Status", Mode},
             {"Parent", Parent}]},
          {data, [{"State", State}]}
         ]
     ],
     {status, self(), {module, Module}, Data}.

application_stopped() ->
    ok.

convert_erlang_start_error({already_started, _}) ->
    {init_failed, "already started"};
convert_erlang_start_error({shutdown, _}) ->
    {init_failed, "shutdown"};
convert_erlang_start_error(Term) ->
    {init_exited, {abnormal, Term}}.
