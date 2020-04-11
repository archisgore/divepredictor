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

-module(computer).
-include_lib("include/divepredictor.hrl").
-export([solve/3]).

%% Upper bound search to 360 days from start
solve(DiveSiteId, StartDate, SolutionCount) ->
	bounded_solve(DiveSiteId, StartDate, SolutionCount, [], 60).

%% Stop when no more days left
bounded_solve(_, _, _, PreviousSolutions, 0) -> PreviousSolutions;

bounded_solve(DiveSiteId, StartDate, DesiredSolutionCount, PreviousSolutions, MaxDays) ->
	DiveSite = divesites:site_by_id(DiveSiteId),
	Tides = tidesandcurrents:get_tides_for_date(StartDate, DiveSite#divesite.noaaTideStationId),
	Currents = tidesandcurrents:get_currents_for_date(StartDate, DiveSite#divesite.noaaCurrentStationId),
	Solutions_finder = DiveSite#divesite.find_solutions,
	case [length(Tides), length(Currents)] of
		[0, 0] ->
			Message = io_lib:format("INTERNAL ERROR: Number of Tides from station ~p was ~p and Currents from station ~p was ~p. " ++ 
				"Both need to be greater than zero to call the solver. Aborting the call. Please fix the NOAA website scraping " ++
				"code or the stationIds to scrape from.", [DiveSite#divesite.noaaTideStationId, length(Tides), 
				DiveSite#divesite.noaaCurrentStationId, length(Currents)]),
			io:fwrite(Message),
			[#divesolution{siteId=DiveSite#divesite.id,time=calendar:universal_time(),description=Message}]++PreviousSolutions;
		_ ->
			Solutions = lists:flatten([PreviousSolutions | Solutions_finder(Tides, Currents)]),
			if length(Solutions) >= DesiredSolutionCount ->	Solutions;
				true -> io:fwrite("Current found ~p solutions of ~p requested. ~n", [length(Solutions), DesiredSolutionCount]),
					bounded_solve(DiveSiteId, edate:shift(StartDate, 1, day), DesiredSolutionCount, Solutions, MaxDays - 1)
			end
	end.
