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

-module(alkifishingreef).
-export([site_info/0]).
-include("include/divepredictor.hrl").

site_info() -> 
	#divesite{
		id="alkifishingreef", 
		name="Alki Fishing Reef",
		noaaTideStationId="9447130",
		noaaCurrentStationId="PCT1681",
		location=#divelocation{address="4503 Beach Dr SW Seattle, WA  98116 United States"},
		find_solutions=fun find_solutions/2

	}.

find_solutions(_, Currents) ->
	io:fwrite("Solutions Finder for Alki Fishing Reef called.~n"),
	[#divesolution{siteId="keystone",time=Slack#current.dateTime, length=-1, 
		description=io_lib:format("Slack between a ~p Exchange", 
				[abs(Before#current.magnitude) + abs(After#current.magnitude)])} 
		||	{Before, Slack, After} <- get_safe_slacks(Currents)].


get_safe_slacks([]) -> [];
get_safe_slacks([First|Rest]) -> get_safe_slacks(First, Rest).

get_safe_slacks(_, []) -> [];
get_safe_slacks(First, [Second| Rest]) -> get_safe_slacks(First, Second, Rest).

get_safe_slacks(_, _, []) -> [];
get_safe_slacks(First, Second, [Third | Rest]) -> 
	lists:flatten([get_possible_solution(First, Second, Third), get_safe_slacks(lists:flatten([Second, Third, Rest]))]).


get_possible_solution(First,Second,Third) -> 
	case [First#current.type, Second#current.type] of
		["flood", "slack"] -> 
			%io:fwrite("Asked to test ~p < ~p < ~p and more~n", [First, Second, Third]),
			case is_convenient(Second) of
				true -> [{First, Second, Third}];
				_ -> [] end;
		_ -> [] end.

is_convenient(Second) ->
	{_, {hour, _, _}} = Second#current.dateTime,
	(hour >= 9) and (hour =< 17).
