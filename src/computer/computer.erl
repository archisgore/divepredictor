-module(computer).
-include_lib("include/divesite.hrl").
-export([solve/3]).

solve(DiveSiteId, StartDate, SolutionCount) ->
	DiveSite = divesites:site_by_id(DiveSiteId),
	
	[#divesolution{siteId=DiveSiteId, time={{2014, 11, 17}, {00, 00, 00}}, length=30}].
