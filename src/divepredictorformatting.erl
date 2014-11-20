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

-module(divepredictorformatting).
-export([integer_to_string_of_length/2, datetime_to_string/1, date_to_string/1, time_to_string/1]).

integer_to_string_of_length(Integer, Length) ->
	string:right(integer_to_list(Integer), Length, $0).

datetime_to_string(DateTime) ->
	{Date, Time} = DateTime,
	date_to_string(Date) ++ " " ++ time_to_string(Time).

date_to_string(Date) -> 
	{Year, Month, Day} = Date,
	divepredictorformatting:integer_to_string_of_length(Year, 4) ++ "-" ++ 
		divepredictorformatting:integer_to_string_of_length(Month, 2) ++ "-" ++ 
		divepredictorformatting:integer_to_string_of_length(Day, 2).

time_to_string(Time) ->
	{Hour, Minute, Second} = Time,
	divepredictorformatting:integer_to_string_of_length(Hour, 2) ++ ":" ++ 
		divepredictorformatting:integer_to_string_of_length(Minute, 2) ++ ":" ++ 
		divepredictorformatting:integer_to_string_of_length(Second, 2).