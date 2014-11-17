-module(computer).
-export(solve/3).

solve(DiveSite, StartDate, SolutionCount) ->
	io:fwrite("Requested solutions for: ~p, ~p, ~p ~n", [DiveSite, StartDate, SolutionCount]).
