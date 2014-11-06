-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").

main() -> 
	[#dtl{
		file="/index.html", 
		folder="priv",
		ext="html", 
		bindings=[
			{template, <<"Hello World">>}
		],
		bind_script=true
	}].