-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").
-include("include/divesite.hrl").

main() -> 
	[#dtl{app=divepredictor,
		bindings=[{form, form()}, {results, results()}]}].

form() -> [#form{body=[#table{class=["standardTable"],
			header=[#tr{cells=[
					#th{body="Dive site"},
					#th{body="Start Date"},
					#th{body="Maximum solutions"}
				]}],

			body=[#tr{cells=[
					#td{body=[#select{name=diveSite, id=diveSite, body=selectable_divesites(), multiple=false, required=true}]},
					#td{body=[#date{name=startDate, id=startDate, required=true, value=selectedStartDate()}]}, 
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
						#submit{id=search, name=search, body="Search Times"}
					]}
				]}]
		}]}].


results() ->
	case {selectedDiveSite(), selectedStartDate(), selectedNumberOfSolutions()} of 
		{_, _, undefined} -> [];
		{_, undefined, _} -> [];
		{undefined, _, _} -> [];
		{DiveSite, StartDate, SolutionCount} -> [#ul{body=solutions(DiveSite, StartDate, SolutionCount)}] end.

solutions((DiveSite, StartDate, SolutionCount)) ->
	<<Year,"-",Month,"-",Day>> = StartDate,
	[#li{body="Diveable at "} | computer:solve(DiveSite, {Year, Month, Day}, list_to_integer(SolutionCount))].

selectable_divesites() ->
	[#option{label=Site#divesite.name, value=Site#divesite.id, selected=(selectedDiveSite() == Site#divesite.id)} || Site <- divesites:list()].


selectedDiveSite() ->
	case wf:qs(<<"diveSite">>) of
		undefined -> 
			[First, _] = divesites:list(),
			First#divesite.id;
		E ->
			F = binary_to_list(E),
			F
	end.

selectedStartDate() -> 
	case wf:qs(<<"startDate">>) of
		undefined -> 
			toDateString(date());
		E ->
			E
	end.

selectedNumberOfSolutions() ->
	case wf:q(<<"numberOfSolutions">>) of
		undefined -> 
			"10";
		E ->
			E
	end.

toDateString(Date) -> 
	{Year, Month, Day} = Date,
	integer_to_string_of_length(Year, 4) ++ "-" ++ integer_to_string_of_length(Month, 2) ++ "-" ++ integer_to_string_of_length(Day, 2).

integer_to_string_of_length(Integer, Length) ->
	string:right(integer_to_list(Integer), Length, $0).