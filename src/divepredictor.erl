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

-module(divepredictor).

-export([start/0]).

start() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(xmerl),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(ranch),
    ok = application:start(cowlib),    
    ok = application:start(cowboy),
    ok = application:start(gproc),
    ok = application:start(edate),
    ok = application:start(merl),
    ok = application:start(erlydtl),
    ok = application:start(erlydtl_runtime),
    ok = application:start(mad_repl),
    ok = application:start(mochieweb_html),
    ok = application:start(mochieweb_xpath),
    ok = application:start(n2o),
    ok = application:start(inets),
    ok = application:start(ssl),
    ok = application:start(divepredictor_app).
