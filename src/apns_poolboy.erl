-module(apns_poolboy).

-export([add_pool/3,
         push/2]).

%% @doc Adds a pool to the started mysql_poolboy application.
add_pool(PoolName, PoolArgs, ApnsArgs) ->
    %% We want strategy fifo as default instead of lifo.
    PoolSpec = child_spec(PoolName, PoolArgs, ApnsArgs),
    supervisor:start_child(apns_poolboy_sup, PoolSpec).

%% @doc Creates a supvervisor:child_spec. When the need to
%% supervise the pools in another way.
child_spec(PoolName, PoolArgs, ApnsArgs) ->
    PoolArgs1 = [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, apns} | PoolArgs],
    poolboy:child_spec(PoolName, PoolArgs1, ApnsArgs).

%% @doc Executes a query to a mysql connection in a given pool.
push(PoolName, Payload) ->
    poolboy:transaction(PoolName, fun(ApnsConn) ->
        apns:push(ApnsConn, Payload)
    end).