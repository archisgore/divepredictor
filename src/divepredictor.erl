-module(divepredictor).

-export([start/0]).

start() ->
    ok = application:start(n2o),
    ok = application:start(divepredictor).