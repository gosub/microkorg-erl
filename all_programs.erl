-module(all_programs).
-export([read_file/1]).

read_file(SysexFile) ->
    {program_data_dump, AllProgsData} = sysex:decode_file(SysexFile).
