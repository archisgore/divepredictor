-module(currents).
-export([url_formatter/2, response_parser/2]).

url_formatter(Date, StationId) ->
	{Year, Month, Day} = Date,
	"http://tidesandcurrents.noaa.gov/noaacurrents/DownloadPredictions?fmt=xml&i=&d=" 
	++ divepredictorformatting:integer_to_string_of_length(Year, 4) ++ "-" 
	++divepredictorformatting:integer_to_string_of_length(Month, 2) ++ "-" 
	++ divepredictorformatting:integer_to_string_of_length(Day, 2) ++ "&r=1&tz=LST%2fLDT&u=1&id=" 
	++ StationId 
	++ "&t=am%2fpm&i=&threshold=&thresholdvalue=".

response_parser(Response, StationId) -> 
	Items = mochiweb_xpath:execute("//data//item",mochiweb_html:parse(Response)),
	[[StationId, getDate(Item), getTime(Item), getType(Item), getMagnitude(Item)] ||
		Item <- Items].

getDate(Item) ->
	{_, _, Elements} = Item,
	[DateTimeElement, _, _] = Elements,
	{<<"datetime">>, [], [DatetimeString]} = DateTimeElement,
	[DateString, _] = string:tokens(binary_to_list(DatetimeString), " "),
	[Year, Month, Day] = string:tokens(DateString, "-"),
	{list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)}.

getTime(Item) ->
	{_, _, Elements} = Item,
	[DateTimeElement, _, _] = Elements,
	{<<"datetime">>, [], [DatetimeString]} = DateTimeElement,
	[_, TimeString] = string:tokens(binary_to_list(DatetimeString), " "),
	[Hour, Minute] = string:tokens(TimeString, ":"),
	{list_to_integer(Hour), list_to_integer(Minute), 0}.

getType(Item) ->
	{_, _, Elements} = Item,
	[_, EventTypeElement, _] = Elements,
	{<<"event">>, [], [EventString]} = EventTypeElement,
	binary_to_list(EventString).

getMagnitude(Item) ->
	{_, _, Elements} = Item,
	[_, _, EventElement] = Elements,
	Type = getType(Item),
	{_, [], [EventMagnitudeString]} = EventElement,
	case Type of
		"slack" -> 0;
		_ -> binary_to_float(EventMagnitudeString)
	end.
