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

-module(keystone).
-export([site_info/0]).
-include("include/divepredictor.hrl").


site_info() -> 
	#divesite{
		id="keystone", 
		name="Keystone Jetty",
		noaaTideStationId="9447905",
		noaaCurrentStationId="PUG1624",
		location=#divelocation{address="12981 WA-20, Coupeville, WA 98239"},
		find_solutions=fun find_solutions/2
	}.

find_solutions(_, Currents) ->
	io:fwrite("Solutions Finder for Keystone called.~n"),
	[#divesolution{siteId="keystone",time=Slack#current.dateTime, length=-1, 
		description=io_lib:format("~p Exchange across ~p minutes: Slack between ~s(~p)@~s - ~s(~p)@~s", 
				[abs(Before#current.magnitude) + abs(After#current.magnitude),
				((calendar:datetime_to_gregorian_seconds(After#current.dateTime) - calendar:datetime_to_gregorian_seconds(Before#current.dateTime)) / 60),
				Before#current.type, Before#current.magnitude, divepredictorformatting:datetime_to_string(Before#current.dateTime),
				After#current.type, After#current.magnitude, divepredictorformatting:datetime_to_string(After#current.dateTime)])} 
		||	{Before, Slack, After} <- get_safe_slacks(Currents)].


get_safe_slacks([]) -> [];
get_safe_slacks([First|Rest]) -> get_safe_slacks(First, Rest).

get_safe_slacks(_, []) -> [];
get_safe_slacks(First, [Second| Rest]) -> get_safe_slacks(First, Second, Rest).

get_safe_slacks(_, _, []) -> [];
get_safe_slacks(First, Second, [Third | Rest]) -> 
	lists:flatten([get_possible_solution(First, Second, Third), get_safe_slacks(lists:flatten([Second, Third, Rest]))]).


get_possible_solution(First,Second,Third) -> 
	case Second#current.type of
		"slack" -> 
			%io:fwrite("Asked to test ~p < ~p < ~p and more~n", [First, Second, Third]),
			case is_safe(First, Third) of
				true -> [{First, Second, Third}];
				_ -> [] end;
		_ -> [] end.

is_safe(First, Third) ->
	Exchange = abs(First#current.magnitude) + abs(Third#current.magnitude),
	Time = (calendar:datetime_to_gregorian_seconds(Third#current.dateTime) - calendar:datetime_to_gregorian_seconds(First#current.dateTime)) / 360.00,
	%io:fwrite("Exchange between ~p(~p)-~p(~p) is ~p (Safe:~p)", [First#current.type, First#current.magnitude,
	%		Third#current.type, Third#current.magnitude, Exchange, Exchange =< 5]),
	(Exchange =< 6) and ((Exchange / Time) =< 0.55).