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

eq_to_map(<<HiFreq:8,HiGain:8,LoFreq:8,LoGain:8>>) ->
    #{hifreq => hifreq_from_int(HiFreq),
     higain => gain_from_int(HiGain),
     lofreq => lofreq_from_int(LoFreq),
     logain => gain_from_int(LoGain)}.

hifreq_from_int(N) ->
    lists:nth(N+1,[1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000,
		  3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 5250,
		  5500, 5750, 6000, 7000, 8000, 9000, 10000, 11000, 12000,
		  14000, 16000, 18000]).

lofreq_from_int(N) ->
    lists:nth(N+1, [40, 50, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240,
		   260, 280, 300, 320, 340, 360, 380, 400, 420, 440, 460,
		   480, 500, 600, 700, 800, 900, 1000]).

gain_from_int(N) when N >= 64-12, N =< 64+12 ->
    N-64.

