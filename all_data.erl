-module(all_data).
-export([read_file/1]).  % to_map/1, from_map/1, read_file/1, write_file/2]).

read_file(SysexFile) ->
    {all_data_dump, AllData} = sysex:decode_file(SysexFile),
    to_map(AllData).

