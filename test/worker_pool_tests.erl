-module(worker_pool_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {inorder,
     {setup,
      fun() -> worker_pool:start() end,
      fun(_) -> skip end,
      [{"add_pool",
        fun() ->
                Fun = fun(_) -> {ok, spawn(fun() -> timer:sleep(100000) end)} end,
                ?assertEqual(ok, worker_pool:add_pool(?MODULE, [a,b,c], Fun)),
                ?assertNotEqual(undefined, worker_pool:whereis(?MODULE, a)),
                ?assertNotEqual(undefined, worker_pool:whereis(?MODULE, b)),
                ?assertNotEqual(undefined, worker_pool:whereis(?MODULE, c)),
                exit(X = worker_pool:whereis(?MODULE, a), ?MODULE),
                timer:sleep(worker_pool:loop_time() * 2),
                ?assertNotEqual(X, worker_pool:whereis(?MODULE, a)),
                ?assertEqual(ok, worker_pool:add_pool(?MODULE, [a,b,c,d], Fun)),
                ?assertNotEqual(undefined, worker_pool:whereis(?MODULE, d)),
                ?assertEqual(ok, worker_pool:add_pool(?MODULE, [a,b,c], Fun)),
                ?assertEqual(undefined, worker_pool:whereis(?MODULE, d)),
                timer:sleep(worker_pool:loop_time() * 2),
                ?assertEqual(undefined, worker_pool:whereis(?MODULE, d)),
                ?assertEqual(3, length(worker_pool:workers(?MODULE)))
        end}
      ]}
    }.

