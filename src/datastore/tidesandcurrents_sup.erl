-module(tidesandcurrents_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case inets:start() of
        ok ->
            io:fwrite("Inets started. Tides and Currents supervisor starting...~n"),
            Child = {tidesandcurrents, {tidesandcurrents, start_link, []}, permanent, brutal_kill, worker, [tidesandcurrents]},
            {ok, {{one_for_one, 5, 10}, [Child]}};
        _ -> io:fwrite("Unable to start internet connectivity.") end.
