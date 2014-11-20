%%******************************************************************************************************************
%%*******************************************************************************************************************
% DivePredictor - A Erlang engine to compute suitability to dive a site, based on prolog-like rules, and
% cached tide/current data from the NOAA.
%
%   Copyright (C) 2014  Archis Gore
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; either version 3 of the License, or
%   (at your option) any later version.
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
%   You should have received a copy of the GNU General Public License
%   along with this program; if not, write to the Free Software Foundation,
%   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
%%******************************************************************************************************************
%%*******************************************************************************************************************/

-module(divesites).
-export([list/0, site_by_id/1]).
-include("include/divesite.hrl").

list() -> [cove2:site_info(), keystone:site_info()].

site_by_id(SiteId) ->
	[First | _] = [Site || Site <- list(), Site#divesite.id == SiteId],
	First.

