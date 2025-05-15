-module(apns_poolboy_sup).

-author("Pankaj Soni <pankajsoni19@live.com>").

-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Pools = application:get_env(apns_poolboy, pools),
    PoolSpec = lists:map(
        fun ({PoolName, {PoolArgs, MysqlArgs}}) ->
            apns_poolboy:child_spec(PoolName, PoolArgs, MysqlArgs)
        end,
        Pools
    ),
    {ok, {{one_for_one, 10, 10}, PoolSpec}}.