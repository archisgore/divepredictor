-module(tidesandcurrents).
-include_lib ("include/divesite.hrl").
-export([get_tides_for_day/2]).
-export([get_currents_for_day/2]).

get_tides_for_day(Date, TideStationId) -> [].
	case fetch_from_database(Date, TideStationId, tides) of
		[] -> 
			Tides = fetch_from_url(Date, TideStationId, tide_url_formatter, tide_response_parser),
			ok = put_into_database(Date, TideStationId, tides),
			Tides;
		E -> E end.


get_currents_for_day(Date, CurrentStationId) ->
	case fetch_from_database(Date, CurrentStationId, currents) of
		[] -> 
			Tides = fetch_from_url(Date, CurrentStationId, current_url_formatter, current_response_parser),
			ok = put_into_database(Date, CurrentStationId, currents),
			Tides;
		E -> E end.

put_into_database(Date, StationId, Table) -> ok.

fetch_from_database(Date, StationId, Table) -> [].

fetch_from_url(Date, StationId, UrlFormatter, ResponseParser) ->

tide_url_formatter(Date, StationId) ->
	{Year, Month, Day} = Date,
	"http://tidesandcurrents.noaa.gov/noaatidepredictions/viewDailyPredictions.jsp?bmon=" ++
		divepredictorformatting:integer_to_string_of_length(Month, 2) ++ 
		"&bday=" ++ divepredictorformatting:integer_to_string_of_length(Day, 2) ++ 
		"&byear=" ++ divepredictorformatting:integer_to_string_of_length(Year, 4) ++
		"&timelength=daily&timeZone=2&dataUnits=1&datum=MLLW&timeUnits=2&interval=highlow&format=Submit&Stationid="
		++ StationId.

tide_response_parser(Response) -> [].

current_url_formatter(Date, StationId) ->
	{Year, Month, Day} = Date,
	"http://tidesandcurrents.noaa.gov/noaacurrents/DownloadPredictions?fmt=xml&i=&d=" 
	++ divepredictorformatting:integer_to_string_of_length(Year, 2) ++ "-" 
	++divepredictorformatting:integer_to_string_of_length(Month, 2) ++ "-" 
	++ divepredictorformatting:integer_to_string_of_length(Day, 2) ++ "&r=1&tz=LST%2fLDT&u=1&id=" 
	++ StationId 
	++ "&t=am%2fpm&i=&threshold=&thresholdvalue=".

current_response_parser(Response) -> [].
