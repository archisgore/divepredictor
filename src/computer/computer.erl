-module(computer).
-include_lib("include/divesite.hrl").
-export([solve/3]).

%% Upper bound search to 360 days from start
solve(DiveSiteId, StartDate, SolutionCount) ->
	solve(DiveSiteId, StartDate, SolutionCount, 360). 

%% Stop when no more days left
solve(_, _, _, 0) -> [];

solve(DiveSiteId, StartDate, SolutionCount, MaxDays) ->
	DiveSite = divesites:site_by_id(DiveSiteId),
	Tides = tidesandcurrents:get_tides_for_date(StartDate, DiveSite#divesite.noaaTideStationId),
	Currents = tidesandcurrents:get_currents_for_date(StartDate, DiveSite#divesite.noaaCurrentStationId),
	io:fwrite("Tides: ~p~n", [Tides]),
	io:fwrite("Currents: ~p~n", [Currents]),
	[#divesolution{siteId=DiveSiteId, time={{2014, 11, 17}, {00, 00, 00}}, length=30}].
