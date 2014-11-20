-module(cove2).
-export([site_info/0]).
-include("include/divepredictor.hrl").

site_info() -> 
	#divesite{
		id="cove2", 
		name="Alki Seacrest Cove 2",
		noaaTideStationId="9447110",
		noaaCurrentStationId="PCT1681",
		location=#divelocation{address="1660 Harbor Ave SW Seattle, WA 98126"},
		find_solutions=fun find_solutions/2

	}.

find_solutions(Tides, Currents) ->
	io:fwrite("Solutions Finder for Cove 2 called.~n"),
	[#divesolution{siteId="cove2",time=date(),length=120}].