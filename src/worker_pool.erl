%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc worker pool
%%% @end
%%%-------------------------------------------------------------------
-module(worker_pool).

%% config
-export([loop_time/0]).

-export([start/0,
         add_pool/3,
         whereis/2,
         workers/1]).

%% callbacks
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% define
%%------------------------------------------------------------------------------
-define(LOOP_TIME, 1000).

-record(state, {name, props = #{}, workers = #{}}).

%%------------------------------------------------------------------------------
%% interface
%%------------------------------------------------------------------------------
-spec start() -> {ok, [atom()]} | {error, any()}.
start() ->
    application:ensure_all_started(?MODULE).

-spec loop_time() -> pos_integer().
loop_time() ->
    application:get_env(?MODULE, loop_time, ?LOOP_TIME).

-spec add_pool(PoolName::atom(), List::[term()], Start::{M::atom(), F::atom()} | fun()) -> ok | {error, any()}.
add_pool(PoolName, List, Start) ->
    case worker_pool_sup:ensure_pool(PoolName, [{start, Start}]) of
        {ok, PID} ->
            case catch gen_server:call(PID, {update_pool, [bind, List]}) of
                ok -> ok;
                {_, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec whereis(PoolName::atom(), Key::term()) -> pid() | undefined.
whereis(PoolName, Key) ->
    case ets:lookup(PoolName, Key) of
        [{_, PID}] when is_pid(PID) -> PID;
        _ -> undefined
    end.

-spec workers(PoolName::atom()) -> [{any(), pid() | undefined}].
workers(PoolName) ->
    ets:tab2list(PoolName).

%%------------------------------------------------------------------------------
-spec start_link(PoolName::atom(), Props::list()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, any()}.
start_link(PoolName, Props) ->
    gen_server:start_link(?MODULE, [PoolName, Props], []).

%% @hidden
init([PoolName, Props]) ->
    process_flag(trap_exit, true),
    ets:new(PoolName, [named_table, public, {read_concurrency, true}]),
    error_logger:info_msg("start worker pool ~p", [{PoolName, Props}]),
    {ok, #state{name = PoolName, props = maps:from_list(Props)}, 0}.

%% @hidden
handle_call({update_pool, Args}, _From, State) ->
    {reply, catch handle_update_pool(Args, State), State};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info(timeout, State) ->
    handle_timeout(State),
    erlang:send_after(loop_time(), self(), timeout),
    {noreply, State};
handle_info({'DOWN', _Ref, process, PID, Reason}, State) ->
     handle_worker_dead(PID, Reason, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden
terminate(Reason, State) ->
    Reason =/= normal andalso
    Reason =/= shutdown andalso
    error_logger:error_msg("worker_pool worker dead ~p~n", [{Reason, State}]).

%%------------------------------------------------------------------------------
handle_update_pool([_, List], #state{name = PoolName, props = #{start := Start}}) ->
    Last = maps:from_list(ets:tab2list(PoolName)),
    UList = lists:usort(List),
    [stop_worker(PoolName, ID, PID) || {ID, PID} <- maps:to_list(Last), not lists:member(ID, UList)],
    [start_worker(PoolName, ID, Start) || ID <- UList, maps:find(ID, Last) =:= error],
    ok.

start_worker(PoolName, ID, Start) ->
    case
        case Start of
            {M, F} when is_atom(M), is_atom(F) -> catch M:F(ID);
            Fun when is_function(Fun) -> catch Fun(ID)
        end of
        {ok, PID} ->
            erlang:monitor(process, PID),
            ets:insert(PoolName, {ID, PID}),
            error_logger:info_msg("worker_pool start worker ~p~n", [{PoolName, Start, PID}]);
        Reason ->
            ets:insert(PoolName, {ID, undefined}),
            error_logger:error_msg("worker_pool start worker fail ~p~n", [{PoolName, Start, Reason}])
    end.


stop_worker(PoolName, ID, PID) ->
    ets:delete(PoolName, ID),
    error_logger:info_msg("worker_pool stop worker ~p~n", [{PoolName, ID, PID}]),
    PID =/= undefined andalso exit(PID, ?MODULE),
    {ID, PID}.

%%------------------------------------------------------------------------------
handle_worker_dead(_PID, ?MODULE, _) -> skip;
handle_worker_dead(PID, Reason, #state{name = Name}) ->
    case ets:match(Name, {'$1', PID}) of
        [[ID]] ->
            ets:insert(Name, {ID, undefined}),
            error_logger:info_msg("worker_pool worker dead ~p~n", [{PID, Name, ID, Reason}]);
        [] ->
            skip
    end.

%%------------------------------------------------------------------------------
handle_timeout(#state{name = Name, props = #{start := Start}}) ->
    case ets:match(Name, {'$1', undefined}) of
        [List] -> [start_worker(Name, ID, Start) || ID <- List];
        [] -> skip
    end.

