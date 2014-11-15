-module(divesites).
-compile(export_all).
-export([list/0]).
-include("include/divesite.hrl").

list() -> [cove2(), keystone_jetty()].

cove2() -> 
	#divesite{id="cove2", name="Alki Seacrest Cove 2"}.

keystone_jetty() -> 
	#divesite{id="keystone", name="Keystone Jetty"}.