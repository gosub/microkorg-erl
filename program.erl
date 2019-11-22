-module(program).
-export([to_map/1, from_map/1, read_file/1, write_file/2,
	 random/0, write_random/1, write_random/0]).

to_map(ProgramData) ->
    program_decode:to_map(ProgramData).

from_map(ProgramMap) ->
    program_encode:from_map(ProgramMap).

read_file(SysexFile) ->
    ProgramData = sysex:decode_file(SysexFile),
    to_map(ProgramData).

write_file(SysexFile, ProgramMap) ->
    ProgramData = from_map(ProgramMap),
    sysex:encode_file(SysexFile, ProgramData).

random() ->
    program_random:generate().

write_random(SysexFile) ->
    write_file(SysexFile, random()).

write_random() ->
    Program = random(),
    #{name := BinName} = Program,
    Name = string:strip(binary_to_list(BinName)) ++ ".syx",
    Res = write_random(Name),
    {Res, Name}.
