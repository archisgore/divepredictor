-module(not_found).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").

main() -> 
	[#dtl{
		file="not_found", 
		ext="html",
		bind_script=true,
		app=divepredictor,
		bindings=[{foo, <<"Foo">>}]
	}].