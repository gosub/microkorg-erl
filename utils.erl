-module(utils).
-export([find/2, list_to_fun/1]).


find(E, L) ->
    find_helper(E, L, 0).

find_helper(_, [], _) ->
    not_found;
find_helper(E, [X|_], N) when E =:= X ->
    {ok, N};
find_helper(E, [_|XS], N) ->
    find_helper(E, XS, N+1).


list_to_fun(List) ->
    fun(N) when is_integer(N) ->
	    lists:nth(N+1, List);
       (A) when is_atom(A) ->
	    {ok, N} = utils:find(A, List),
	    N
    end.
