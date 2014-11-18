-module(tidesandcurrents).
-behavior(gen_server).
-include_lib ("include/divesite.hrl").
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
	case fetch_from_database(Date, TideStationId, tides) of
		[] -> 
			Tides = fetch_from_url(Date, TideStationId, fun tide_url_formatter/2, fun tide_response_parser/1),
			ok = put_into_database(Date, TideStationId, tides),
			{reply, Tides, State};
		E -> {reply, E, State} end;

handle_call({currents, Date, CurrentStationId}, _From, State) ->
	case fetch_from_database(Date, CurrentStationId, currents) of
		[] -> 
			Tides = fetch_from_url(Date, CurrentStationId, fun current_url_formatter/2, fun current_response_parser/1),
			ok = put_into_database(Date, CurrentStationId, currents),
			{reply, Tides, State};
		E -> {reply, E, State} end.

put_into_database(Date, StationId, Table) -> ok.

fetch_from_database(Date, StationId, Table) -> [].

fetch_from_url(Date, StationId, UrlFormatter, ResponseParser) ->
	Url = UrlFormatter(Date, StationId),
	io:fwrite("Calling URL: ~ts~n", [Url]),
	{ok, {{_, ResponseCode, ReasonPhrase}, _, Body}} = httpc:request(Url),
	case ResponseCode of
		200 -> ResponseParser(Body);
		_ -> io:fwrite("Received response ~p, Reason ~p. Aborting data fetch.~n", [ResponseCode, ReasonPhrase]),
			[] end.

tide_url_formatter(Date, StationId) ->
	{Year, Month, Day} = Date,
	"http://tidesandcurrents.noaa.gov/noaatidepredictions/viewDailyPredictions.jsp?bmon=" ++
		divepredictorformatting:integer_to_string_of_length(Month, 2) ++ 
		"&bday=" ++ divepredictorformatting:integer_to_string_of_length(Day, 2) ++ 
		"&byear=" ++ divepredictorformatting:integer_to_string_of_length(Year, 4) ++
		"&timelength=daily&timeZone=2&dataUnits=1&datum=MLLW&timeUnits=2&interval=highlow&format=Submit&Stationid="
		++ StationId.

tide_response_parser(Response) -> 
	%%io:fwrite("Response recieved for Tide URL: ~ts~n", [Response]),
	[].

current_url_formatter(Date, StationId) ->
	{Year, Month, Day} = Date,
	"http://tidesandcurrents.noaa.gov/noaacurrents/DownloadPredictions?fmt=xml&i=&d=" 
	++ divepredictorformatting:integer_to_string_of_length(Year, 4) ++ "-" 
	++divepredictorformatting:integer_to_string_of_length(Month, 2) ++ "-" 
	++ divepredictorformatting:integer_to_string_of_length(Day, 2) ++ "&r=1&tz=LST%2fLDT&u=1&id=" 
	++ StationId 
	++ "&t=am%2fpm&i=&threshold=&thresholdvalue=".

current_response_parser(Response) -> 
	io:fwrite("Response recieved for Current URL: ~ts~n", [Response]),
	[].



%%==============================================================================
%% Server Functions
%%==============================================================================

init([]) ->
	process_flag(trap_exit, true),
	io:fwrite("TidesAndCurrents server starting... ~p (~p) ~n", [{local, ?MODULE}, self()]),
	{ok, []}.

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