-module(all_programs).
-export([read_file/1]).


read_file(SysexFile) ->
    {program_data_dump, AllProgsData} = sysex:decode_file(SysexFile),
    lists:map(fun program:to_map/1, separate_programs(AllProgsData)).


separate_programs(<<>>) ->
    [];
separate_programs(<<Program:254/bytes,Rest/bytes>>) ->
    [Program | separate_programs(Rest)].
