-module(program).
-export([to_map/1, from_map/1]).

to_map(ProgramData) ->
    program_decode:to_map(ProgramData).

from_map(TODO) ->
    TODO.
