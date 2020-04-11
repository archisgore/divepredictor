%%******************************************************************************************************************
%%*******************************************************************************************************************
% DivePredictor - A Erlang engine to compute suitability to dive a site, based on prolog-like rules, and
% cached tide/current data from the NOAA.
%
%   Copyright (C) 2014  Archis Gore
%    This file is part of DivePredictor.
%
%    DivePredictor is free software: you can redistribute it and/or modify
%    it under the terms of the GNU Affero General Public License as
%    published by the Free Software Foundation, either version 3 of the
%    License, or (at your option) any later version.
%
%    DivePredictor is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU Affero General Public License for more details.
%
%    You should have received a copy of the GNU Affero General Public License
%    along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%******************************************************************************************************************
%%*******************************************************************************************************************/

-module(index).
-compile(export_all).
-export([main/0]).
-include_lib("n2o/include/wf.hrl").
-include("include/divepredictor.hrl").

main() -> 
	[#dtl{app=divepredictor,
		bindings=[{form, form()}, 
			{results, results()}, 
			{analytics, os:getenv("GOOGLE_ANALYTICS")}, 
			{ads, os:getenv("GOOGLE_ADS")}]}].

form() -> [#form{body=[#table{class=["standardTable"],
			header=[#tr{cells=[
					#th{body="Dive site"},
					#th{body="Start Date"},
					#th{body="Minimum solutions (upto 2 months from Start Date)"}
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
		{DiveSiteId, StartDate, SolutionCount} -> 
			[#table{body=[#tr{cells=[
				#td{body=[#span{body=google_map(DiveSiteId)}]},
				#td{body=[#ul{body=solutions_list_items(DiveSiteId, StartDate, SolutionCount)}]}
			]}]}] end.

selectable_divesites() ->
	[#option{body=Site#divesite.name, value=Site#divesite.id, selected=(selectedDiveSite() == Site#divesite.id)} || Site <- divesites:list()].


selectedDiveSite() ->
	case wf:qs(<<"diveSite">>) of
		undefined -> 
			[First| _] = divesites:list(),
			First#divesite.id;
		E ->
			binary_to_list(E)
	end.

selectedStartDate() -> 
	case wf:qs(<<"startDate">>) of
		undefined -> 
			divepredictorformatting:date_to_string(date());
		E ->
			E
	end.

selectedNumberOfSolutions() ->
	case wf:q(<<"numberOfSolutions">>) of
		undefined -> 
			"1";
		E ->
			binary_to_list(E)
	end.

solutions_list_items(DiveSiteId, StartDate, SolutionCount) ->
	[Year,Month,Day] = re:split(StartDate, "-"),
	[#li{body=solution_to_text(Solution)} || Solution <- solutions(DiveSiteId, Year, Month, Day, SolutionCount)].

solution_to_text(Solution) ->
	DiveSite = divesites:site_by_id(Solution#divesolution.siteId),
	io_lib:format("~s divable at <b>~s</b> ~s (Details: <i>~s</i>)", [DiveSite#divesite.name, 
			divepredictorformatting:datetime_to_string(Solution#divesolution.time), 
			solution_suitability_length(Solution#divesolution.length), Solution#divesolution.description]).

solution_suitability_length(-1) -> "";
solution_suitability_length(Length) ->
	io_lib:format("suitable for <b>~p minutes</b>", [Length]).

solutions(DiveSiteId, Year, Month, Day, SolutionCount) ->
		 computer:solve(DiveSiteId, {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, list_to_integer(SolutionCount)).

google_map(DiveSiteId) ->
	DiveSite = divesites:site_by_id(DiveSiteId),
	[#iframe{
		width=600, 
	  	height=450,
	  	style="border",
	  	src=google_map_uri(DiveSite#divesite.location)}].


google_map_uri(Location) ->
	case {Location#divelocation.latitude, Location#divelocation.longitude} of
		{-1, -1} -> "https://www.google.com/maps/embed/v1/place?key=AIzaSyD9JtvXxQOJAb5c0R0qrNGIRu-AC7DUZDw&zoom=18&q=" ++ Location#divelocation.address;
		{_, _} -> "https://www.google.com/maps/embed/v1/view?key=AIzaSyD9JtvXxQOJAb5c0R0qrNGIRu-AC7DUZDw&zoom=18&center="
			++ Location#divelocation.latitude ++ "," ++ Location#divelocation.longitude 
	end.
