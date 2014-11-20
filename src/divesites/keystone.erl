-module(keystone).
-export([site_info/0]).
-include("include/divepredictor.hrl").


site_info() -> 
	#divesite{
		id="keystone", 
		name="Keystone Jetty",
		noaaTideStationId="9447905",
		noaaCurrentStationId="PCT1491",
		location=#divelocation{address="1390 Washington 20, Oak Harbor, WA 98277"},
		find_solutions=fun find_solutions/2
	}.

find_solutions(Tides, Currents) ->
	io:fwrite("Solutions Finder for Keystone called.~n"),
	[].