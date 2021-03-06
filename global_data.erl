-module(global_data).
-export([to_map/1, from_map/1, read_file/1, write_file/2]).


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
	 MidiInPrgChgMap:128/bytes>>)
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
      midi1_ctrl_no => Midi1CtrlNo,
      midi2_ctrl_no => Midi2CtrlNo,
      systemex_filter => dis_ena(SystemExFilter),
      note_receive => note_receive(NoteReceive),
      pbend_filter => dis_ena(PBendFilter),
      ctrlchg_filter => dis_ena(CtrlChgFilter),
      progchg_filter => dis_ena(ProgChgFilter),
      ctrlchange_no => ctrlchange_no(CtrlChangeNo),
      user_scale => user_scale(UserScale),
      midi_in_progchg_map => midi_in_progchg_map(MidiInPrgChgMap)}.

position(0) -> postkbd;
position(1) -> pretg;
position(postkbd) -> 0;
position(pretg) -> 1.

velcurve(8) -> const;
velcurve(X) -> X+1.

velcurve_inverse(const) -> 8;
velcurve_inverse(X) -> X-1.

clock(0) -> internal;
clock(1) -> external;
clock(2) -> auto;
clock(internal) -> 0;
clock(external) -> 1;
clock(auto) -> 2.

ctrl_no(-1) -> off;
ctrl_no(off) -> -1;
ctrl_no(X) -> X.

dis_ena(0) -> disabled;
dis_ena(1) -> enabled;
dis_ena(disabled) -> 0;
dis_ena(enabled) -> 1.

note_receive(0) -> all;
note_receive(all) -> 0;
note_receive(X) -> X.

ctrlchange_no(Data) when is_binary(Data) ->
    [ctrl_no(B) || <<B:8>> <= Data];
ctrlchange_no(List) when is_list(List) ->
    list_to_binary(lists:map(fun(C) -> ctrl_no(C) end, List)).

user_scale(Data) when is_binary(Data) ->
    [X || <<X/signed-integer>> <= Data];
user_scale(List) when is_list(List) ->
    list_to_binary([<<X/signed-integer>> || X <- List]).

midi_in_progchg_map(Data) when is_binary(Data) ->
    [num2prog(B) || <<B:8>> <= Data];
midi_in_progchg_map(List) when is_list(List) ->
    list_to_binary([prog2num(P) || P <- List]).


num2prog(N) when N < 64 ->
    num2prog(N, "A");
num2prog(N) ->
    num2prog(N-64, "b").

num2prog(N, L) ->
    Tens = (N div 8) + 1,
    Units = (N rem 8) + 1,
    X = Tens*10+Units,
    S = L ++ integer_to_list(X),
    list_to_atom(S).

prog2num(Atom) when is_atom(Atom) ->
    prog2num(atom_to_list(Atom));
prog2num([$A|Numeric]=L) when is_list(L) ->
    N = list_to_integer(Numeric),
    prog2num(N);
prog2num([$b|Numeric]=L) when is_list(L) ->
    N = list_to_integer(Numeric),
    prog2num(N) + 64;
prog2num(N) when is_integer(N) ->
    Octs = (N div 10) - 1,
    Units = (N rem 10) - 1,
    Octs*8 + Units.


write_file(SysexFile, GlobalDataMap) ->
    GlobalDataBin = from_map(GlobalDataMap),
    sysex:encode_file(SysexFile, global_data_dump, GlobalDataBin).


from_map(#{master_tune := MasterTune, transpose := Transpose,
	   position := Position, vel_value := VelValue,
	   vel_curve := VelCurve, local_ctrl := LocalCtrl,
	   memory_protect := MemoryProtect, clock := Clock,
	   midi_ch := MidiCh, sync_ctrl_no := SyncCtrlNo,
	   timbsel_ctrl_no := TimbSelCtrlNo,
	   midi1_ctrl_no := Midi1CtrlNo,
	   midi2_ctrl_no := Midi2CtrlNo,
	   systemex_filter := SystemExFilter,
	   note_receive := NoteReceive,
	   pbend_filter := PBendFilter,
	   ctrlchg_filter := CtrlChgFilter,
	   progchg_filter := ProgChgFilter,
	   ctrlchange_no := CtrlChangeNo,
	   user_scale := UserScale,
	   midi_in_progchg_map := MidiInPrgChgMap}) ->
    <<(round((MasterTune-440)*10)):8/signed-integer,
      Transpose:8/signed-integer, 0:7, (position(Position)):1,
      VelValue:8, (velcurve_inverse(VelCurve)):8,
      0:5, (enums:onoff(LocalCtrl)):1, 0:1, (enums:onoff(MemoryProtect)):1,
      0:22, (clock(Clock)):2, 0:4, (MidiCh-1):4,
      (ctrl_no(SyncCtrlNo)):8/signed-integer,
      (ctrl_no(TimbSelCtrlNo)):8/signed-integer,
      0:16, Midi1CtrlNo:8, Midi2CtrlNo:8,
      (dis_ena(SystemExFilter)):1, 0:5, (note_receive(NoteReceive)):2,
      0:1, (dis_ena(PBendFilter)):1, 0:3, (dis_ena(CtrlChgFilter)):1,
      0:1, (dis_ena(ProgChgFilter)):1, (ctrlchange_no(CtrlChangeNo)):42/bytes,
      (user_scale(UserScale)):12/bytes,
      (midi_in_progchg_map(MidiInPrgChgMap)):128/bytes>>.
