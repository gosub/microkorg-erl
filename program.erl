-module(program).
-export([to_map/1]).

to_map(ProgramData) ->
    <<Name:12/bytes, _:16, ArpCtrlData:2/bytes,
      _:2, VoiceMode:2, 0:4, ScaleKey:4, ScaleType:4, _:8,
      DelayFx:4/bytes, ModFx:3/bytes, Eq:4/bytes, Arp:7/bytes,
      Rest/bytes>> = ProgramData,
    #{name => Name,
      arpctrl => arpctrl_to_map(ArpCtrlData),
      voice_mode => voice_mode_from_int(VoiceMode),
      scale_key => scale_key_from_int(ScaleKey),
      scale_type => ScaleType,
      delayfx => delayfx_to_map(DelayFx),
      modfx => modfx_to_map(ModFx),
      eq => eq_to_map(Eq),
      arp => arp_to_map(Arp),
      rest => Rest}.

arpctrl_to_map(<<0:5, Len:3, T1:1,T2:1,T3:1,T4:1,T5:1,T6:1,T7:1,T8:1>>) ->
    #{len => Len+1, pattern => [T1,T2,T3,T4,T5,T6,T7,T8]}.

voice_mode_from_int(0) -> single;
voice_mode_from_int(2) -> double;
voice_mode_from_int(3) -> vocoder.

scale_key_from_int(N) ->
    lists:nth(N+1, [c,cs,d,ds,e,f,fs,g,gs,a,as,b]).

delayfx_to_map(<<>>) ->
    todo.

modfx_to_map(<<>>) ->
    todo.

eq_to_map(<<>>) ->
    todo.

arp_to_map(<<>>) ->
    todo.
