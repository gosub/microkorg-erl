-module(utils).
-export([find/2, list_to_fun/1, rnd/1, deep_random_merge/2]).


rnd(N) when is_integer(N) ->
    rand:uniform(N);
rnd(T) when is_tuple(T) ->
    element(rnd(tuple_size(T)), T).


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

deep_random_merge(A, B) when is_map(A), is_map(B) ->
    Keys = maps:keys(A),
    maps:from_list([{Key, deep_random_merge(maps:get(Key, A), maps:get(Key, B))} || Key <- Keys]);
deep_random_merge([], []) -> [];
deep_random_merge([], B) when is_list(B) ->
    B;
deep_random_merge(A, []) when is_list(A) ->
    A;
deep_random_merge([HeadA | RestA], [HeadB | RestB]) ->
    [deep_random_merge(HeadA, HeadB) | deep_random_merge(RestA, RestB)];
deep_random_merge(A, B) ->
    rnd({A, B}).
