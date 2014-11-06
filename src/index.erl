-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").

main() -> 
	#span{body="Hello"}.