-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").
-include("include/divesite.hrl").

main() -> 
	[#dtl{
		file="index", 
		ext="html",
		bind_script=true,
		app=divepredictor,
		bindings=[{content, content()}]
	}].

content() ->
	[
		#form{body=[#table{class=["standardTable"],
			header=[#tr{cells=[
					#th{body="Dive site"},
					#th{body="Start Date"},
					#th{body="End Date (Optional)"},
					#th{body="Maximum solutions (Optional)"}
				]}],

			body=[#tr{cells=[
					#td{body=[#select{body=selectable_divesites(), multiple=false, required=true, id=diveSite}]},
					#td{body=[#date{id=startDate, required=true, value=selectedStartDate()}, 
						#input_time{id=startTime, required=true, value=selectedStartTime()}]},
					#td{body=[#date{id=endDate, required=false, value=selectedEndDate()}, 
						#input_time{id=endTime, required=false, value=selectedEndTime()}]},
					#td{body=[#number{id=numberOfSolutions, required=true, step=1, max=100, min=1, value=selectedNumberOfSolutions()}]}
				]}],

			footer=[
				#tr{cells=[
					#td{colspan="4", body=[
						#span{body="*Times are interpreted in the timezone of where the divesite is.", class=["italic"]}
					]}
				]},
				#tr{cells=[
					#td{colspan="4", class=["centered"], body=[
						#submit{body="Search Times"}
					]}
				]}]
		}]}
	].


event(search) ->
	[#span{body="Hello!"}].

selectable_divesites() ->
	[#option{label=Site#divesite.name, value=Site#divesite.id, selected=(selectedDiveSite() == Site#divesite.id)} || Site <- divesites:list()].

selectedStartDate() -> [].

selectedStartTime() -> [].

selectedEndDate() -> [].

selectedEndTime() -> [].

selectedNumberOfSolutions() -> [].

selectedDiveSite() -> [].