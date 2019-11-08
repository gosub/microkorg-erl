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

timbre(#{midi_ch:=MidiCh, assign_mode:=AssignMode, eg2_reset:=EG2Reset,
	 eg1_reset:=EG1Reset, trigger_mode:=TrigMode, key_priority:=KeyPrio,
	 unison_detune:=UniDet, pitch:=Pitch, osc1:=Osc1, osc2:=Osc2,
	 porta_time:=Porta, mixer:=Mixer, filter:=Filter}) ->
    MidiChData = midich(MidiCh),
    AssignModeData = enums:timbre_assign(AssignMode),
    EG2ResetData = enums:onoff(EG2Reset),
    EG1ResetData = enums:onoff(EG1Reset),
    TrigModeData = enums:timbre_trigger(TrigMode),
    KeyPrioData = keypriority(KeyPrio),
    PitchData = pitch(Pitch),
    Osc1Data = osc1(Osc1),
    Osc2Data = osc2(Osc2),
    MixerData = mixer(Mixer),
    FilterData = filter(Filter),
    <<MidiChData/signed-integer, AssignModeData:2, EG2ResetData:1,
      EG1ResetData:1, TrigModeData:1, 0:1, KeyPrioData:2, UniDet:8,
      PitchData:4/bytes,Osc1Data:5/bytes,Osc2Data:3/bytes, 0:1,
      Porta:7, MixerData:3/bytes,FilterData:6/bytes>>.

%% timbre_to_map(<<MidiCh/signed-integer,
%% 		AssignMode:2, EG2Reset:1, EG1Reset:1, TriggerMode:1,
%% 	      _:1, KeyPriority:2, UnisonDetune:8, Pitch:4/bytes, Osc1:5/bytes,
%% 	      Osc2:3/bytes, 0:1, PortamentoTime:7, Mixer:3/bytes,
%% 	      Filter:6/bytes, Amp:5/bytes, EG1:4/bytes, EG2:4/bytes,
%% 	      LFO1:3/bytes, LFO2:3/bytes, Patch:8/bytes, _:56/bytes>>)
%%   when UnisonDetune =< 99 ->
%%     #{midi_ch => timbre_midich(MidiCh),
%%       assign_mode => enums:timbre_assign(AssignMode),
%%       eg2_reset => enums:onoff(EG2Reset),
%%       eg1_reset => enums:onoff(EG1Reset),
%%       trigger_mode => enums:timbre_trigger(TriggerMode),
%%       key_priority => timbre_keypriority(KeyPriority),
%%       unison_detune => UnisonDetune,
%%       pitch => timbre_pitch(Pitch),
%%       osc1 => timbre_osc1(Osc1),
%%       osc2 => timbre_osc2(Osc2),
%%       porta_time => PortamentoTime,
%%       mixer => timbre_mixer(Mixer),
%%       filter => timbre_filter(Filter),
%%       amp => timbre_amp(Amp),
%%       eg1 => timbre_eg(EG1),
%%       eg2 => timbre_eg(EG2),
%%       lfo1 => timbre_lfo(1, LFO1),
%%       lfo2 => timbre_lfo(2, LFO2),
%%       patch => timbre_patch(Patch)}.

midich(global) -> -1;
midich(N) when N >= 0 -> N.

keypriority(last) -> 0;
keypriority(N) -> N.

pitch(#{tune:=Tune, bend:=Bend, transpose:=Trans, vibrato:=Vibr})
  when abs(Tune) =< 50, abs(Bend) =< 12,
       abs(Trans) =< 24, abs(Vibr) =< 63 ->
    TuneData = Tune + 64,
    BendData = Bend + 64,
    TransData = Trans + 64,
    VibrData = Vibr + 64,
    <<TuneData:8, BendData:8, TransData:8, VibrData:8>>.

osc1(#{wave:=Wave, ctrl1:=Ctrl1, ctrl2:=Ctrl2, dwgs:=DWGS}) ->
    WaveData = enums:timbre1_wave(Wave),
    DWGSData = DWGS-1,
    <<WaveData:8, Ctrl1:8, Ctrl2:8, DWGSData:8, 0:8>>.

osc2(#{modselect:=ModSelect, wave:=Wave, semitone:=Semi,tune:=Tune})
  when abs(Semi) =< 24, abs(Tune) =< 63 ->
    ModSelData = enums:timbre2_modselect(ModSelect),
    WaveData = enums:timbre2_wave(Wave),
    SemiData = Semi+64,
    TuneData = Tune+64,
    <<0:2,ModSelData:2,0:2,WaveData:2,SemiData:8,TuneData:8>>.

mixer(#{osc1_lvl:=Osc1Lvl, osc2_lvl:=Osc2Lvl, noise:=Noise}) ->
    <<Osc1Lvl:8,Osc2Lvl:8,Noise:8>>.

filter(#{type:=Type,cutoff:=Cutoff,reso:=Reso,eg1_intensity:=EG1Int,
	 velocity_sense:=VelSens, key_track:=KeyTrack})
  when abs(EG1Int) =< 63, abs(KeyTrack) =< 63 ->
    TypeData = enums:filter_type(Type),
    EG1IntData = EG1Int + 64,
    VelSensData = VelSens + 64,
    KeyTrackData = KeyTrack + 64,
    <<TypeData:8, Cutoff:8, Reso:8, EG1IntData:8,
      VelSensData:8, KeyTrackData:8>>.

vocoder(_) ->
    <<>>.
