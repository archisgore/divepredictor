-module(divepredictor_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    case cowboy:start_http(http, 100, [{port, wf:config(n2o,port,port())}],
                                    [{env, [{dispatch, dispatch_rules()}]}]) of
        {ok, _} -> ok;
        {error,{{_,{_,_,{X,_}}},_}} -> io:format("Can't start Web Server: ~p\r\n",[X]), halt(abort,[]);
        X -> io:format("Unknown Error: ~p\r\n",[X]), halt(abort,[]) end,
    {ok, {{one_for_one, 5, 10}, []}}.

mime() -> [{mimetypes,cow_mimetypes,all}].

dispatch_rules() ->
    cowboy_router:compile(
        [{'_', [
            {"/static/[...]", n2o_dynalo, {dir, "priv/static", mime()}},
            {"/n2o/[...]", n2o_dynalo, {dir, "deps/n2o/priv", mime()}},
            {'_', n2o_cowboy, []}
    ]}]).

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.