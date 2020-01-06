-module(all_programs).
-export([read_file/1, write_file/2,
	 to_list/1, from_list/1,
	 random/0, write_random/1]).


read_file(SysexFile) ->
    {program_data_dump, AllProgsData} = sysex:decode_file(SysexFile),
    to_list(AllProgsData).


to_list(AllProgsData) ->
    lists:map(fun program:to_map/1, separate_programs(AllProgsData)).


write_file(ProgramList, SysexFile) when length(ProgramList) == 128 ->
    Binary = from_list(ProgramList),
    sysex:encode_file(SysexFile, program_data_dump, Binary).


from_list(ProgramList) when length(ProgramList) == 128 ->
    Binaries = lists:map(fun program:from_map/1, ProgramList),
    list_to_binary(Binaries).

random() ->
    NonVocoders = [program:random() || _ <- lists:seq(1, (128-16))],
    Vocoders = [program:random_vocoder() || _ <-lists:seq(1, 16)],
    NonVocoders ++ Vocoders.


write_random(SysexFile) ->
    write_file(random(), SysexFile).


separate_programs(<<>>) ->
    [];
separate_programs(<<Program:254/bytes,Rest/bytes>>) ->
    [Program | separate_programs(Rest)].
