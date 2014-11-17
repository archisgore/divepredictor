-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").
-include("include/divesite.hrl").

main() -> 
	[#dtl{app=divepredictor,
		bindings=[{content, content()}]}].

content() -> [#form{body=[#table{class=["standardTable"],
			header=[#tr{cells=[
					#th{body="Dive site"},
					#th{body="Start Date"},
					#th{body="End Date (Optional)"},
					#th{body="Maximum solutions (Optional)"}
				]}],

			body=[#tr{cells=[
					#td{body=[#select{name=diveSite, id=diveSite, body=selectable_divesites(), multiple=false, required=true}]},
					#td{body=[#date{name=startDate, id=startDate, required=true, value=selectedStartDate()}, 
						#input_time{name=startTime, id=startTime, required=true, value=selectedStartTime()}]},
					#td{body=[#date{name=endDate, id=endDate, required=false, value=selectedEndDate()}, 
						#input_time{name=endTime, id=endTime, required=false, value=selectedEndTime()}]},
					#td{body=[#number{name=numberOfSolutions, id=numberOfSolutions, required=true, step=1, max=100, min=1, value=selectedNumberOfSolutions()}]}
				]}],

			footer=[
				#tr{cells=[
					#td{colspan="4", body=[
						#span{body="*Times are interpreted in the timezone of where the divesite is.", class=["italic"]}
					]}
				]},
				#tr{cells=[
					#td{colspan="4", class=["centered"], body=[
						#button{id=search, name=search, body="Search Times", 
							source=[diveSite, startDate, startTime, endDate, endTime, numberOfSolutions],
							postback=search}
					]}
				]}]
		}]}].


event(search) ->
	wf:insert_bottom([#span{body="Hello!"}]).

selectable_divesites() ->
	[#option{label=Site#divesite.name, value=Site#divesite.id, selected=(selectedDiveSite() == Site#divesite.id)} || Site <- divesites:list()].

selectedStartDate() -> 
	case wf:q(startDate) of
		undefined -> toDateString(calendar:universal_time());
		E -> E
	end.

selectedStartTime() ->
	case wf:q(startTime) of
		undefined -> toTimeString(calendar:universal_time());
		E -> E
	end.

selectedEndDate() ->
	case wf:q(endDate) of
		undefined -> toDateString(calendar:universal_time());
		E -> E
	end.


selectedEndTime() ->
	case wf:qs(endTime) of
		undefined -> toTimeString(calendar:universal_time());
		E -> E
	end.

selectedNumberOfSolutions() ->
	case wf:q(numberOfSolutions) of
		undefined -> "10";
		E -> E
	end.

selectedDiveSite() ->
	case wf:q(diveSite) of
		undefined -> 
			[First, Rest] = divesites:list(),
			First#divesite.id;
		E -> E
	end.


toDateString(Date) -> "2014-11-05".

toTimeString(Time) -> "12:01".