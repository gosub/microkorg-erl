-module(global_data).
-export([to_map/1, read_file/1]).


read_file(SysexFile) ->
    {global_data_dump, GlobalData} = sysex:decode_file(SysexFile),
    to_map(GlobalData).


to_map(<<MasterTune:8/signed-integer,
	 Transpose:8/signed-integer,
	 0:7, Position:1,
	 _Rest:195/bytes>>)
    #{master_tune => MasterTune/10 + 440,
      transpose => Transpose,
      position => position(Position)}.

position(0) -> postkbd;
position(1) -> pretg.
