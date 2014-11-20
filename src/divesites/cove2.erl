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

-module(cove2).
-export([site_info/0]).
-include("include/divepredictor.hrl").

site_info() -> 
	#divesite{
		id="cove2", 
		name="Alki Seacrest Cove 2",
		noaaTideStationId="9447110",
		noaaCurrentStationId="PCT1681",
		location=#divelocation{address="1660 Harbor Ave SW Seattle, WA 98126"},
		find_solutions=fun find_solutions/2

	}.

find_solutions(Tides, Currents) ->
	io:fwrite("Solutions Finder for Cove 2 called.~n"),
	[#divesolution{siteId="cove2",time=Current#current.dateTime,length=-1, 
		description=io_lib:format("~p (~p) - Cove 2 is always divable", [Current#current.type, Current#current.magnitude])} || 
			Current <- Currents].