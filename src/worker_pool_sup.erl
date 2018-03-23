%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc worker pool
%%% @end
%%%-------------------------------------------------------------------
-module(worker_pool_sup).

-export([start_link/0,
         ensure_pool/2,
         init/1]).

%%------------------------------------------------------------------------------
-behaviour(supervisor).

%%------------------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec ensure_pool(PoolName::atom(), Props::list()) -> {ok, pid()} | {error, any()}.
ensure_pool(PoolName, Props) ->
    case which_pool(PoolName) of
        undefined -> start_pool(PoolName, Props);
        PID -> {ok, PID}
    end.

start_pool(PoolName, Props) ->
    supervisor:start_child(?MODULE, {PoolName,
                                     {worker_pool, start_link, [PoolName, Props]},
                                     transient, infinity, worker,
                                     []}).

which_pool(PoolName) ->
    List = [{ID, PID} || {ID, PID, _, _} <- supervisor:which_children(?MODULE)],
    case maps:find(PoolName, maps:from_list(List)) of
        error -> undefined;
        {ok, PID} -> PID
    end.

-spec init([]) -> supervisor:init().
init([]) ->
    {ok, {{one_for_one, 1, 60}, []}}.

