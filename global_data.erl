-module(global_data).
-export([to_map/1, read_file/1]).


read_file(SysexFile) ->
    {global_data_dump, GlobalData} = sysex:decode_file(SysexFile),
    to_map(GlobalData).


to_map(<<MasterTune:8/signed-integer,
	 Transpose:8/signed-integer,
	 0:7, Position:1,
	 VelValue:8, VelCurve:8,
	 _:5, LocalCtrl:1, _:1, MemoryProtect:1,
	 _:16, 0:6, Clock:2,
	 _Rest:191/bytes>>)
  when VelValue >= 1, VelValue =< 127,
       VelCurve =< 8 ->
    #{master_tune => MasterTune/10 + 440,
      transpose => Transpose,
      position => position(Position),
      vel_value => VelValue,
      vel_curve => velcurve(VelCurve),
      local_ctrl => enums:onoff(LocalCtrl),
      memory_protect => enums:onoff(MemoryProtect),
      clock => clock(Clock)}.

position(0) -> postkbd;
position(1) -> pretg.

velcurve(8) -> const;
velcurve(X) -> X+1.

clock(0) -> internal;
clock(1) -> external;
clock(2) -> auto.
