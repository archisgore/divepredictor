-module(divesites).
-export([list/0, site_by_id/1]).
-include("include/divesite.hrl").

list() -> [cove2(), keystone_jetty()].

site_by_id(SiteId) ->
	[First | _] = [Site || Site <- list(), Site#divesite.id == SiteId],
	First.

cove2() -> 
	#divesite{
		id="cove2", 
		name="Alki Seacrest Cove 2",
		noaaTideStationId="9447110",
		noaaCurrentStationId="PCT1681",
		location=#divelocation{address="1660 Harbor Ave SW Seattle, WA 98126"}
	}.

keystone_jetty() -> 
	#divesite{
		id="keystone", 
		name="Keystone Jetty",
		noaaTideStationId="9447905",
		noaaCurrentStationId="PCT1491",
		location=#divelocation{address="1390 Washington 20, Oak Harbor, WA 98277"}
	}.
