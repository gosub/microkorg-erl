-module(all_programs).
-export([read_file/1, write_file/2]).


read_file(SysexFile) ->
    {program_data_dump, AllProgsData} = sysex:decode_file(SysexFile),
    lists:map(fun program:to_map/1, separate_programs(AllProgsData)).


write_file(ProgramList, SysexFile) when length(ProgramList) == 128 ->
    Binaries = lists:map(fun program:from_map/1, ProgramList),
    Binary = list_to_binary(Binaries),
    sysex:encode_file(SysexFile, program_data_dump, Binary).


separate_programs(<<>>) ->
    [];
separate_programs(<<Program:254/bytes,Rest/bytes>>) ->
    [Program | separate_programs(Rest)].
