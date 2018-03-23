%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc worker pool
%%% @end
%%%-------------------------------------------------------------------
-module(worker_pool_app).

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
-behaviour(application).

%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    worker_pool_sup:start_link().

stop(_State) ->
    ok.

