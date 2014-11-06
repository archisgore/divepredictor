-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").

main() -> 
	[head(), body()].

head() ->
	#head{body=title()}.

title() ->
	#title{body="Dive Site Finder"}.

body() ->
	#body{body="Hello World!"}.