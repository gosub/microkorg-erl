-module(global_data).
-export([to_map/1, read_file/1]).


read_file(SysexFile) ->
    ok.
%    {program_data_dump, AllProgsData} = sysex:decode_file(SysexFile),
%    lists:map(fun program:to_map/1, separate_programs(AllProgsData)).


to_map(GlobalData) ->
    ok.
