-module(all_data).
-export([read_file/1, write_file/2, to_map/1, from_map/1]).

read_file(SysexFile) ->
    {all_data_dump, AllData} = sysex:decode_file(SysexFile),
    to_map(AllData).

write_file(AllData, SysexFile) ->
    Binary = from_map(AllData),
    sysex:encode_file(SysexFile, all_data_dump, Binary).

to_map(<<AllProgramData:32512/bytes, GlobalData:200/bytes>>) ->
    #{programs => all_programs:to_list(AllProgramData),
      global_data => global_data:to_map(GlobalData)}.

from_map(#{global_data := GD, programs := Prgs}) ->
    <<(all_programs:from_list(Prgs)):32512/bytes,
      (global_data:from_map(GD)):200/bytes>>.
