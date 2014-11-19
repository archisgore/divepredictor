%%******************************************************************************************************************
%%*******************************************************************************************************************
% DivePredictor - A Erlang engine to compute suitability to dive a site, based on prolog-like rules, and
% cached tide/current data from the NOAA.
%
%   Copyright (C) 2014  Archis Gore
%    This file is part of DivePredictor.
%
%    DivePredictor is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    DivePredictor is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
%%******************************************************************************************************************
%%*******************************************************************************************************************/

-record(divelocation, {
		latitude=-1,
		longitude=-1,
		address=""
	}).

-record(divesite, {
		id,
		name,
		noaaTideStationId=0,
		noaaCurrentStationId=0,
		location=#divelocation{}
	}).

-record(divesolution, {
	siteId,
	time,
	length
	}).

-record(current, {
		stationId,
		dateTime,
		type,
		magnitude
	}).

-record(tide, {
		stationId,
		dateTime,
		type,
		magnitude
	}).