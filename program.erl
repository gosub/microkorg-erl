-module(program).
-export([to_map/1]).

bin2onoff(0) -> off;
bin2onoff(1) -> on.

to_map(ProgramData) ->
    <<Name:12/bytes, _:16, ArpCtrlData:2/bytes,
      _:2, VoiceMode:2, 0:4, ScaleKey:4, ScaleType:4, _:8,
      DelayFx:4/bytes, ModFx:3/bytes, Eq:4/bytes, Arp:7/bytes,
      KbdOctave/signed-integer, VoicesParams:216/bytes>> = ProgramData,
    Mode = voice_mode_from_int(VoiceMode),
    #{name => Name,
      arpctrl => arpctrl_to_map(ArpCtrlData),
      voice_mode => Mode,
      scale_key => scale_key_from_int(ScaleKey),
      scale_type => ScaleType,
      delayfx => delayfx_to_map(DelayFx),
      modfx => modfx_to_map(ModFx),
      eq => eq_to_map(Eq),
      arp => arp_to_map(Arp),
      kbd_oct => KbdOctave,
      voices => voices_to_list(Mode, VoicesParams)}.

arpctrl_to_map(<<0:5, Len:3, T1:1,T2:1,T3:1,T4:1,T5:1,T6:1,T7:1,T8:1>>) ->
    #{len => Len+1, pattern => [T1,T2,T3,T4,T5,T6,T7,T8]}.

voice_mode_from_int(0) -> single;
voice_mode_from_int(2) -> double;
voice_mode_from_int(3) -> vocoder.

scale_key_from_int(N) ->
    lists:nth(N+1, [c,cs,d,ds,e,f,fs,g,gs,a,as,b]).

delayfx_to_map(<<Sync:1, 0:3, TimeBase:4,Time:8,Depth:8,Type:8>>) ->
    #{sync => bin2onoff(Sync),
      timebase => delay_timebase_from_int(TimeBase),
      time => Time,
      depth => Depth,
      type => delay_type_from_int(Type)}.

delay_timebase_from_int(N) ->
    lists:nth(N+1, ['1/32','1/24','1/16','1/12','3/32','1/8','1/6',
		    '3/16','1/4','1/3','3/8','1/2','2/3','3/4','1/1']).

eq_to_map(<<>>) ->
    todo.
delay_type_from_int(0) -> stereo;
delay_type_from_int(1) -> cross;
delay_type_from_int(2) -> lr.

arp_to_map(<<>>) ->
    todo.
modfx_to_map(<<LFOSpeed:8,Depth:8,Type:8>>) ->
    #{lfo_speed => LFOSpeed,
     depth => Depth,
     type => mod_type_from_int(Type)}.

mod_type_from_int(0) -> chorus;
mod_type_from_int(1) -> ensemble;
mod_type_from_int(2) -> phaser.
