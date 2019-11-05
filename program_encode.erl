-module(program_encode).
-export([from_map/1]).

from_map(#{name:=Name, arpctrl:=ArpCtrl, voice_mode:=VoiceMode,
	   scale_key:=ScaleKey, scale_type:=ScaleType, delayfx:=DelayFx,
	   modfx:=ModFx, eq:=Eq, arp:=Arp, kbd_oct:=KbdOctave,
	   voices:=Voices}) ->
    ArpCtrlData = arpctrl(ArpCtrl),
    ScaleKeyData = enums:scale_key(ScaleKey),
    VoiceModeData = enums:voice_mode(VoiceMode),
    DelayFxData = delayfx(DelayFx),
    ModFxData = modfx(ModFx),
    EqData = eq(Eq),
    ArpData = arp(Arp),
    VoicesData = list_to_voices(VoiceMode, Voices),
    <<Name:12/bytes, 0:16, ArpCtrlData:2/bytes, 0:2,
      VoiceModeData:2, 0:4, ScaleKeyData:4, ScaleType:4, 0:8,
      DelayFxData:4/bytes, ModFxData:3/bytes, EqData:4/bytes,
      ArpData:7/bytes, KbdOctave/signed-integer, VoicesData:216/bytes>>.

%% to_map(ProgramData) ->
%%     <<Name:12/bytes, _:16, ArpCtrlData:2/bytes,
%%       _:2, VoiceMode:2, 0:4, ScaleKey:4, ScaleType:4, _:8,
%%       DelayFx:4/bytes, ModFx:3/bytes, Eq:4/bytes, Arp:7/bytes,
%%       KbdOctave/signed-integer, VoicesParams:216/bytes>> = ProgramData,
%%     Mode = enums:voice_mode(VoiceMode),
%%     #{name => Name,
%%       arpctrl => arpctrl(ArpCtrlData),
%%       voice_mode => Mode,
%%       scale_key => enums:scale_key(ScaleKey),
%%       scale_type => ScaleType,
%%       delayfx => delayfx(DelayFx),
%%       modfx => modfx(ModFx),
%%       eq => eq(Eq),
%%       arp => arp(Arp),
%%       kbd_oct => KbdOctave,
%%       voices => voices_to_list(Mode, VoicesParams)}.

arpctrl(#{len:=Len, pattern:=[T1,T2,T3,T4,T5,T6,T7,T8]}) ->
    LenDec = Len - 1,
    <<0:5, LenDec:3, T1:1,T2:1,T3:1,T4:1,T5:1,T6:1,T7:1,T8:1>>.

delayfx(#{sync:=Sync, timebase:=TimeBase, time:=Time, depth:=Depth,
	  type:=Type}) ->
    SyncData = enums:onoff(Sync),
    TimeBaseData = enums:delay_timebase(TimeBase),
    TypeData = enums:delay_type(Type),
    <<SyncData:1, 0:3, TimeBaseData:4, Time:8, Depth:8, TypeData:8>>.

modfx(#{lfo_speed:=LFOSpeed, depth:=Depth, type:=Type}) ->
    TypeData = enums:mod_type(Type),
    <<LFOSpeed:8, Depth:8, TypeData:8>>.

eq(#{hifreq:=HiFreq, higain:=HiGain,
     lofreq:=LoFreq, logain:=LoGain}) ->
    HiFreqData = enums:hifreqs(HiFreq),
    HiGainData = gain(HiGain),
    LoFreqData = enums:lofreqs(LoFreq),
    LoGainData = gain(LoGain),
    <<HiFreqData:8, HiGainData:8,
      LoFreqData:8, LoGainData:8>>.

gain(N) when abs(N) =< 12 -> N+64.

arp(#{tempo:=Tempo, onoff:=OnOff, latch:=Latch, target:=Target,
      keysync:=KeySync, range:=Range, type:=Type, gate_time:=GateTime,
      resolution:=Reso, swing:=Swing})
  when GateTime =< 100, abs(Swing) =< 100 ->
    OnOffData = enums:onoff(OnOff),
    LatchData = enums:onoff(Latch),
    TargetData = enums:arp_target(Target),
    KeySyncData = enums:onoff(KeySync),
    RangeData = Range-1,
    TypeData = enums:arp_type(Type),
    ResoData = enums:arp_reso(Reso),
    <<Tempo:16, OnOffData:1, LatchData:1, TargetData:2, 0:3, KeySyncData:1,
      RangeData:4, TypeData:4, GateTime:8, ResoData:8, Swing/signed-integer>>.

list_to_voices(single, [TimbreMap1]) ->
    TimbreData = timbre(TimbreMap1),
    <<TimbreData:108/bytes, TimbreData:108/bytes>>;
list_to_voices(double, [TimbreMap1, TimbreMap2]) ->
    Timbre1Data = timbre(TimbreMap1),
    Timbre2Data = timbre(TimbreMap2),
    <<Timbre1Data:108/bytes, Timbre2Data:108/bytes>>;
list_to_voices(vocoder, [VocoderMap]) ->
    VocoderData = vocoder(VocoderMap),
    Zero = 0,
    <<VocoderData:104/bytes, Zero:112/bytes>>.

timbre(_) ->
    <<>>.

vocoder(_) ->
    <<>>.
