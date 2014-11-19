-module(tidesandcurrents_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
        Worker = {tidesandcurrents, {tidesandcurrents, start_link, []}, permanent, brutal_kill, worker, [tidesandcurrents]},
        {ok, {{one_for_one, 5, 10}, [Worker]}}.
