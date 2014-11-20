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
-module(tidesandcurrents).
-behavior(gen_server).
-include_lib ("include/divepredictor.hrl").
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_tides_for_date/2, get_currents_for_date/2]).

%%=================================================================================================================================
%% Gen Server stuff
%%=================================================================================================================================
get_tides_for_date(Date, TideStationId) ->
	gen_server:call(?MODULE, {tides, Date, TideStationId}, 2000).

get_currents_for_date(Date, CurrentStationId) ->
	gen_server:call(?MODULE, {currents, Date, CurrentStationId}, 2000).

%%=================================================================================================================================
%% Private functions to do processing
%%=================================================================================================================================

handle_call({tides, Date, TideStationId}, _From, State) ->
	case fetch_from_database(tides, Date, TideStationId) of
		[] -> 
			Tides = fetch_from_url(Date, TideStationId, fun tides:url_formatter/2, fun tides:response_parser/2),
			ok = put_into_database(tides, Tides),
			{reply, to_tides_records(Tides), State};
		E -> {reply, to_tides_records(E), State} end;

handle_call({currents, Date, CurrentStationId}, _From, State) ->
	case fetch_from_database(currents, Date, CurrentStationId) of
		[] -> 
			Currents = fetch_from_url(Date, CurrentStationId, fun currents:url_formatter/2, fun currents:response_parser/2),
			ok = put_into_database(currents, Currents),
			{reply, to_currents_records(Currents), State};
		E -> {reply, to_currents_records(E), State} end.

to_currents_records(Currents) ->
	[#current{stationId=StationId, dateTime={Date, Time}, type=Type, magnitude=Magnitude} ||  [StationId, Date, Time, Type, Magnitude] <- Currents].

to_tides_records(Tides) ->
	[#tide{stationId=StationId, dateTime={Date, Time}, type=Type, magnitude=Magnitude} ||  [StationId, Date, Time, Type, Magnitude] <- Tides].

put_into_database(Table, Entries) -> 
	ok = database:store(io_lib:format("INSERT INTO ~s VALUES ($1, $2, $3, $4, $5);", [Table]), to_database_entries(Entries)).

fetch_from_database(Table, Date, StationId) -> 
	from_database_entries(database:fetch(lists:flatten(io_lib:format("SELECT * FROM ~s WHERE id='~s' AND day='~s';", 
		[Table, StationId, to_date_string(Date)])))).

fetch_from_url(Date, StationId, UrlFormatter, ResponseParser) ->
	Url = UrlFormatter(Date, StationId),
	io:fwrite("Calling URL: ~ts~n", [Url]),
	{ok, {{_, ResponseCode, ReasonPhrase}, _, Body}} = httpc:request(Url),
	case ResponseCode of
		200 -> ResponseParser(Body, StationId);
		_ -> io:fwrite("Received response ~p, Reason ~p. Aborting data fetch.~n", [ResponseCode, ReasonPhrase]),
			[] end.

to_date_string({Year, Month, Day}) ->
	lists:flatten(io_lib:format("~s/~s/~s", 
		[divepredictorformatting:integer_to_string_of_length(Year,4), 
			divepredictorformatting:integer_to_string_of_length(Month,2),
			divepredictorformatting:integer_to_string_of_length(Day,2)])).

to_time_string({Hour, Minute, Second}) ->
	lists:flatten(io_lib:format("~s:~s:~s", 
		[divepredictorformatting:integer_to_string_of_length(Hour,2), 
			divepredictorformatting:integer_to_string_of_length(Minute,2),
			divepredictorformatting:integer_to_string_of_length(Second,2)])).

%% Convert to Database date times
to_database_entries(Entries) ->
	[ [StationId, to_date_string(Date), to_time_string(Time), Type, Magnitude] || [StationId, Date, Time, Type, Magnitude] <- Entries].

%% Convert from binary strings to strings
from_database_entries(Entries) ->
	[[binary_to_list(StationId), Date, Time, binary_to_list(Type), Magnitude] || 
		{StationId, Date, Time, Type, Magnitude} <- Entries].

%%==============================================================================
%% Server Functions
%%==============================================================================

init([]) ->
	process_flag(trap_exit, true),
	io:fwrite("TidesAndCurrents server starting... ~p (~p) ~n", [{local, ?MODULE}, self()]),
	{ok, []}.

handle_cast(_, _) ->
	io:fwrite("Asynchronous requests not supported ~p", [{local, ?MODULE}]),
	ok.

handle_info(Info, State) ->
	io:fwrite("Timed out ~p", [{local, ?MODULE}]),
	{noreply, Info, State}.

terminate(_, _) ->
	io:fwrite("Terminating ~p", [{local, ?MODULE}]),
	ok.

code_change(_, _, _) -> ok.


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).