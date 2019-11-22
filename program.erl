-module(program).
-export([to_map/1, from_map/1, read_file/1, write_file/2, write_file/1,
	 random/0, write_random/1, write_random/0, set_name/2]).

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

write_file(ProgramMap) ->
    #{name := Name} = ProgramMap,
    Filename = string:strip(binary_to_list(Name)) ++ ".syx",
    write_file(Filename, ProgramMap).

random() ->
    program_random:generate().

write_random(SysexFile) ->
    write_file(SysexFile, random()).

write_random() ->
    Program = random(),
    #{name := BinName} = Program,
    Name = string:strip(binary_to_list(BinName)) ++ ".syx",
    Res = write_file(Name, Program),
    {Res, Name}.

set_name(Program, NewName)
  when is_map(Program), is_list(NewName) ->
    NewNameMap = #{name => list_to_binary(string:pad(NewName, 12))},
    maps:merge(Program, NewNameMap).
