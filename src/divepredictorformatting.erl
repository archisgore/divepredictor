-module(divepredictorformatting).
-export([integer_to_string_of_length/2]).

integer_to_string_of_length(Integer, Length) ->
	string:right(integer_to_list(Integer), Length, $0).
