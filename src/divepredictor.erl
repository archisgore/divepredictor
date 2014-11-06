-module(divepredictor).

-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(xmerl),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(ranch),
    ok = application:start(cowlib),    
    ok = application:start(cowboy),
    ok = application:start(gproc),
    ok = application:start(erlydtl),
    ok = application:start(n2o),
    ok = application:start(divepredictor).