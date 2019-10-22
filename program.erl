-module(program).
-export([to_map/1]).

to_map(ProgramData) ->
    <<Name:12/bytes, _:16, Rest/bytes>> = ProgramData,
    #{name => Name, rest => Rest}.
