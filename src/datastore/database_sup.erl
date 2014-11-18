-module(database_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:fwrite("Database supervisor starting... ~p (~p) ~n", [{local, ?MODULE}, self()]),
    Child = {database, {database, start_link, []}, permanent, brutal_kill, worker, [database]},
    {ok, {{one_for_one, 5, 10}, [Child]}}.
