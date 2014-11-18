-module(database).
-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%==============================================================================
%% Client API
%%==============================================================================
store(Table, Key, Data) ->
	io:fwrite("Data storage request ~p~n", [Data]),
	gen_server:call(?MODULE, {"Hello World"}).


fetch(Table, Key) ->
	[].

%%==============================================================================
%% Server Functions
%%==============================================================================

init([]) ->
	process_flag(trap_exit, true),
	io:fwrite("Database server starting... ~p (~p) ~n", [{local, ?MODULE}, self()]),
	connect().


handle_call(Request, _, PgsqlConnection) ->
	io:fwrite("Handling call. ~p~n", [Request]).

handle_cast(_, _) ->
	io:fwrite("Asynchronous requests not supported.~n"),
	ok.

handle_info(Info, State) ->
	io:fwrite("Timed out ~p", [{local, ?MODULE}]),
	{noreply, Info, State}.

terminate(_, _) ->
	io:fwrite("Terminating ~p", [{local, ?MODULE}]),
	ok.

code_change(_, _, _) -> ok.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


connect() ->
	Username = os:getenv("DB_USERNAME"),
	Password = os:getenv("DB_PASSWORD"),
	Host = os:getenv("DB_HOST"),
	Database = os:getenv("DB_DATABASE"),
	Port = list_to_integer(os:getenv("DB_PORT")),
	io:fwrite("Database connection parameters: Username:~s, Password:~s, Host:~s, Database:~s, Port:~p~n", [Username, Password, Host, Database, Port]),
	%%pgsql:connect(Host, Username, Password, [{"database", Database},{"port", Port}]).
	{ok, "C"}.

