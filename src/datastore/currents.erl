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
