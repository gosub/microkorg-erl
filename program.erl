-module(program).
-export([to_map/1, from_map/1, read_file/1]).

to_map(ProgramData) ->
    program_decode:to_map(ProgramData).

from_map(ProgramMap) ->
    program_encode:from_map(ProgramMap).

read_file(SysexFile) ->
    ProgramData = sysex:decode_file(SysexFile),
    to_map(ProgramData).
