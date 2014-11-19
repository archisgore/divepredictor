-module(tides).
-export([url_formatter/2, response_parser/2]).


url_formatter(Date, StationId) ->
	{Year, Month, Day} = Date,
	"http://tidesandcurrents.noaa.gov/noaatidepredictions/viewDailyPredictions.jsp?bmon=" ++
		divepredictorformatting:integer_to_string_of_length(Month, 2) ++ 
		"&bday=" ++ divepredictorformatting:integer_to_string_of_length(Day, 2) ++ 
		"&byear=" ++ divepredictorformatting:integer_to_string_of_length(Year, 4) ++
		"&timelength=daily&timeZone=2&dataUnits=1&datum=MLLW&timeUnits=2&interval=highlow&format=Submit&Stationid="
		++ StationId.

response_parser(Response, StationId) -> 
	Html = mochiweb_html:parse(Response),
	EntryRows = mochiweb_xpath:execute("//form[@name='dataform']//table//table//tr", Html),
	BeginDate = mochiweb_xpath:execute("//input[@name='bdate']", Html),
	[{_, BeginAttributes, _}] = BeginDate,
	[BeginDateString|_] = [BeginDateString || {<<"value">>, BeginDateString} <- BeginAttributes],
	Year = list_to_integer(lists:sublist(binary_to_list(BeginDateString), 4)),
	[parse_entry(EntryRow, Year, StationId) || EntryRow <- EntryRows].

parse_entry(EntryRow, Year, StationId) ->
	{_, _, Cols} = EntryRow,
	[DateCol, _, TimeCol, MagnitudeCol] = Cols,
	[StationId, parse_date(DateCol, Year), parse_time(TimeCol), parse_type(MagnitudeCol), parse_magnitude(MagnitudeCol)].


parse_date(DateCol, Year) ->
	{_, _, [DateText]} = DateCol,
	[Month, Day] = string:tokens(binary_to_list(DateText), "/"),
	{Year, list_to_integer(Month), list_to_integer(Day)}.

parse_time(TimeCol) ->
	{_, _, [TimeTextRaw]} = TimeCol,
	TimeText = string:strip(string:strip(binary_to_list(TimeTextRaw), both), both, $\n),
	[_, Time, AMPM] = string:tokens(TimeText, " "),
	[Hour, Minute] = string:tokens(Time, ":"),
	{list_to_integer(Hour) + offset_for_24_hours(AMPM), list_to_integer(Minute), 0}.

offset_for_24_hours("AM") -> 0;
offset_for_24_hours("PM") -> 12.

parse_type(MagnitudeCol) ->
	{_, _, [MagnitudeTextRaw]} = MagnitudeCol,
	[_, TypeText] = string:tokens(string:strip(binary_to_list(MagnitudeTextRaw), both), " "),
	case string:strip(TypeText, both, $\n) of
		"H" -> "HighTide";
		"L" -> "LowTide"
	end.

parse_magnitude(MagnitudeCol) ->
	{_, _, [MagnitudeTextRaw]} = MagnitudeCol,
	[MagnitudeText, _] = string:tokens(string:strip(binary_to_list(MagnitudeTextRaw), both), " "),
	list_to_float(string:strip(MagnitudeText, both, $\n)).

