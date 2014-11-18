-module(database).
-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%==============================================================================
%% Client API
%%==============================================================================
store(Data) ->
	io:fwrite("Data storage request ~p~n", [Data]),
	gen_server:call(?MODULE, {"Hello World"}).

%%==============================================================================
%% Server Functions
%%==============================================================================

init([]) ->
	process_flag(trap_exit, true),
	io:fwrite("Database server starting... ~p (~p) ~n", [{local, ?MODULE}, self()]),
	{ok, []}.

handle_call(Request, _, _) ->
	io:fwrite("Handling call. ~p~n", [Request]).

handle_cast(_, _) ->
	ok.

handle_info(Info, State) ->
	io:fwrite("Timed out ~p", [{local, ?MODULE}]),
	{noreply, Info, State}.

terminate(_, _) ->
	io:fwrite("Terminating ~p", [{local, ?MODULE}]),
	ok.

code_change(_, _, _) -> ok.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).