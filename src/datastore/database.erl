%%******************************************************************************************************************
%%*******************************************************************************************************************
% DivePredictor - A Erlang engine to compute suitability to dive a site, based on prolog-like rules, and
% cached tide/current data from the NOAA.
%
%   Copyright (C) 2014  Archis Gore
%    This file is part of DivePredictor.
%
%    DivePredictor is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    DivePredictor is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%******************************************************************************************************************
%%*******************************************************************************************************************/

-module(database).
-behavior(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([store/2, fetch/1]).

%%==============================================================================
%% Client API
%%==============================================================================
store(Query, Data) ->
	gen_server:call(?MODULE, {store, Query, Data}).


fetch(Query) ->
	gen_server:call(?MODULE, {fetch, Query}).

%%==============================================================================
%% Server Functions
%%==============================================================================

init([]) ->
	process_flag(trap_exit, true),
	io:fwrite("Database server starting... ~p (~p) ~n", [{local, ?MODULE}, self()]),
	Conn = connect(),
	io:fwrite("Database connection open ~p~n", [Conn]),
	ok = ensure_tables_exist(Conn),
	{ok, Conn}.

handle_call({fetch, Query}, _, Conn) ->
	%io:fwrite("Fetch Request ~s~n", [Query]),
	{_, ResList} = pgsql_connection:simple_query(Query, Conn),
	{reply, ResList, Conn};

handle_call({store, Query, [First|Rest]}, _From, Conn) ->
	case pgsql_connection:extended_query(Query, First, Conn) of
		{{insert,0,1},[]} -> handle_call({store, Query, Rest}, _From, Conn);
		E -> io:fwrite("~p~n Aborting further inserts", [E]),
			{reply, ok, Conn}
	end;

handle_call({store, _, []}, _, Conn) ->
	{reply, ok, Conn}.

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

start_link() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


connect() ->
	Username = os:getenv("DB_USERNAME"),
	Password = os:getenv("DB_PASSWORD"),
	Host = os:getenv("DB_HOST"),
	Database = os:getenv("DB_DATABASE"),
	Port = list_to_integer(os:getenv("DB_PORT")),
	io:fwrite("Database connection parameters: Username:~s, Password:~s, Host:~s, Database:~s, Port:~p~n", [Username, Password, Host, Database, Port]),
	pgsql_connection:open(Host, Database, Username, Password, [{"port", Port}, {ssl, true}]).

ensure_tables_exist(Conn) ->
	ok = ensure_tides_table(Conn),
	ok = ensure_currents_table(Conn),
	ok.

ensure_tides_table(Conn) ->
	case pgsql_connection:sql_query("CREATE TABLE IF NOT EXISTS tides 
			(id TEXT, day DATE, time TIME, type TEXT, magnitude FLOAT, 
				CONSTRAINT tides_primary_key PRIMARY KEY (id, day, time));", Conn) of
		{updated, 1} ->	ok;
		E -> {error, E} end.

ensure_currents_table(Conn) ->
	case pgsql_connection:sql_query("CREATE TABLE IF NOT EXISTS currents 
				(id TEXT, day DATE, time TIME, type TEXT, magnitude FLOAT, 
					CONSTRAINT currents_primary_key PRIMARY KEY (id, day, time));", Conn) of
		{updated, 1} -> ok;
		E -> {error, E} end.
