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
	 _:16, 0:6, Clock:2, 0:4, MidiCh:4,
	 SyncCtrlNo:8/signed-integer,
	 TimbSelCtrlNo:8/signed-integer,
	 _:16, Midi1CtrlNo:8, Midi2CtrlNo:8,
	 SystemExFilter:1, 0:5, NoteReceive:2,
	 0:1, PBendFilter:1, 0:3, CtrlChgFilter:1, 0:1, ProgChgFilter:1,
	 CtrlChangeNo:42/bytes, UserScale:12/bytes,
	 _Rest:128/bytes>>)
  when VelValue >= 1, VelValue =< 127, VelCurve =< 8,
       SyncCtrlNo >= -1, SyncCtrlNo =< 95,
       TimbSelCtrlNo >= -1, TimbSelCtrlNo =< 95 ->
    #{master_tune => MasterTune/10 + 440,
      transpose => Transpose,
      position => position(Position),
      vel_value => VelValue,
      vel_curve => velcurve(VelCurve),
      local_ctrl => enums:onoff(LocalCtrl),
      memory_protect => enums:onoff(MemoryProtect),
      clock => clock(Clock),
      midi_ch => MidiCh+1,
      sync_ctrl_no => ctrl_no(SyncCtrlNo),
      timbsel_ctrl_no => ctrl_no(TimbSelCtrlNo),
      midi1_ctrl_no => ctrl_no(Midi1CtrlNo),
      midi2_ctrl_no => ctrl_no(Midi2CtrlNo),
      systemex_filter => dis_ena(SystemExFilter),
      note_receive => note_receive(NoteReceive),
      pbend_filter => dis_ena(PBendFilter),
      ctrlchg_filter => dis_ena(CtrlChgFilter),
      progchg_filter => dis_ena(ProgChgFilter),
      ctrlchange_no => ctrlchange_no(CtrlChangeNo),
      user_scale => user_scale(UserScale)}.

position(0) -> postkbd;
position(1) -> pretg.

velcurve(8) -> const;
velcurve(X) -> X+1.

clock(0) -> internal;
clock(1) -> external;
clock(2) -> auto.

ctrl_no(-1) -> off;
ctrl_no(X) -> X.

dis_ena(0) -> disabled;
dis_ena(1) -> enabled.

note_receive(0) -> all;
note_receive(X) -> X.

ctrlchange_no(Data) ->
    [ctrl_no(B) || <<B:8>> <= Data].

user_scale(Data) ->
    [X || <<X/signed-integer>> <= Data].
