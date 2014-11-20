%%******************************************************************************************************************
%%*******************************************************************************************************************
% DivePredictor - A Erlang engine to compute suitability to dive a site, based on prolog-like rules, and
% cached tide/current data from the NOAA.
%
%   Copyright (C) 2014  Archis Gore
%    This file is part of DivePredictor.
%
%    DivePredictor is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    DivePredictor is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
%%******************************************************************************************************************
%%*******************************************************************************************************************/

-module(computer).
-include_lib("include/divepredictor.hrl").
-export([solve/3]).

%% Upper bound search to 360 days from start
solve(DiveSiteId, StartDate, SolutionCount) ->
	solve(DiveSiteId, StartDate, SolutionCount, [], 360). 

%% Stop when no more days left
solve(_, _, _, PreviousSolutions, 0) -> PreviousSolutions;

solve(DiveSiteId, StartDate, DesiredSolutionCount, PreviousSolutions, MaxDays) ->
	DiveSite = divesites:site_by_id(DiveSiteId),
	Tides = tidesandcurrents:get_tides_for_date(StartDate, DiveSite#divesite.noaaTideStationId),
	Currents = tidesandcurrents:get_currents_for_date(StartDate, DiveSite#divesite.noaaCurrentStationId),
	solutions_finder = DiveSite#divesite.find_solutions,
	Solutions = [PreviousSolutions | [solutions_finder(Tides, Currents)]],
	if length(Solutions) >= DesiredSolutionCount ->	Solutions;
		true -> solve(DiveSiteId, edate:shift(StartDate, 1, day), DesiredSolutionCount, Solutions, MaxDays - 1)
	end.
