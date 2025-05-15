-module(apns_poolboy_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, apns_poolboy_sup}, apns_poolboy_sup, []).

stop(_State) ->
    ok.