-module(divepredictor).

-export([start/0]).

start() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(xmerl),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(ranch),
    ok = application:start(cowlib),    
    ok = application:start(cowboy),
    ok = application:start(gproc),
    ok = application:start(erlydtl),
    ok = application:start(n2o),
    ok = application:start(inets),
    ok = application:start(ssl),
    ok = application:start(divepredictor).