-module(program_encode).
%-export([from_map/1]).
-compile(export_all).
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
  when Tempo >= 20, Tempo =< 300, GateTime =< 100, abs(Swing) =< 100 ->
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
    <<VocoderData:104/bytes, 0:(112*8)>>.

timbre(#{midi_ch:=MidiCh, assign_mode:=AssignMode, eg2_reset:=EG2Reset,
	 eg1_reset:=EG1Reset, trigger_mode:=TrigMode, key_priority:=KeyPrio,
	 unison_detune:=UniDet, pitch:=Pitch, osc1:=Osc1, osc2:=Osc2,
	 porta_time:=Porta, mixer:=Mixer, filter:=Filter, amp:=Amp,
	 eg1:=EG1, eg2:=EG2, lfo1:=LFO1, lfo2:=LFO2, patch:=Patch})
  when UniDet =< 99 ->
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
    AmpData = amp(Amp),
    EG1Data = eg(EG1),
    EG2Data = eg(EG2),
    LFO1Data = lfo(1, LFO1),
    LFO2Data = lfo(2, LFO2),
    PatchData = patch(Patch),
    <<MidiChData/signed-integer, AssignModeData:2, EG2ResetData:1,
      EG1ResetData:1, TrigModeData:1, 0:1, KeyPrioData:2, UniDet:8,
      PitchData:4/bytes,Osc1Data:5/bytes,Osc2Data:3/bytes, 0:1,
      Porta:7, MixerData:3/bytes, FilterData:6/bytes, AmpData:5/bytes,
      EG1Data:4/bytes, EG2Data:4/bytes, LFO1Data:3/bytes, LFO2Data:3/bytes,
      PatchData:8/bytes, 0:(56*8)>>.

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

amp(#{level:=Level, pan:=Pan, sw:=SW, distortion:=Dist,
      velocity_sense:=VelSense, key_track:=KeyTrack})
  when abs(KeyTrack) =< 63 ->
    PanData = Pan + 64,
    DistData = enums:onoff(Dist),
    VelSenseData = VelSense + 64,
    KeyTrackData = KeyTrack + 64,
    <<Level:8, PanData:8, 0:1, SW:1, 0:5, DistData:1,
      VelSenseData:8, KeyTrackData:8>>.

eg(#{attack:=Attack, decay:=Decay, sustain:=Sustain, release:=Release}) ->
    <<Attack:8, Decay:8, Sustain:8, Release:8>>.

lfo(N, #{keysync:=KeySync, wave:=Wave, freq:=Freq, tempo_sync:=TempoSync,
	 sync_note:=SyncNote}) ->
    KeySyncData = enums:lfo_keysync(KeySync),
    WaveData = enums:lfo_wave(N, Wave),
    TempoSyncData = enums:onoff(TempoSync),
    SyncNoteData = enums:lfo_syncnote(SyncNote),
    <<0:2, KeySyncData:2, 0:2, WaveData:2, Freq:8, TempoSyncData:1,
      0:2, SyncNoteData:5>>.

patch({Cable1, Cable2, Cable3, Cable4}) ->
    Cable1Data = cable(Cable1),
    Cable2Data = cable(Cable2),
    Cable3Data = cable(Cable3),
    Cable4Data = cable(Cable4),
    <<Cable1Data:2/bytes, Cable2Data:2/bytes,
      Cable3Data:2/bytes, Cable4Data:2/bytes>>.

cable(#{destination:=Dest, source:=Source, intensity:=Intesity})
  when abs(Intesity) =< 63 ->
    DestData = enums:cable_dest(Dest),
    SourceData = enums:cable_source(Source),
    IntData = Intesity +64,
    <<DestData:4, SourceData:4, IntData:8>>.

vocoder(#{midi_ch:=MidiCh, assign_mode:=AssignMode, eg2_reset:=EG2Reset,
	  eg1_reset:=EG1Reset, trigger_mode:=TrigMode, key_priority:=KeyPrio,
	  unison_detune:=UniDet, pitch:=Pitch, osc1:=Osc1,
	  audioin1_hpfgate:=AudioIn1HPFGate, porta_time:=Porta, mixer:=Mixer,
	  audioin1:=AudioIn1, filter:=Filter, amp:=Amp, eg2:=EG2, lfo1:=LFO1,
	  lfo2:=LFO2, ch_levels:=ChLevels, pan_levels:=PanLevels,
	  hold_levels:=HoldLevels}) ->
    <<(midich(MidiCh))/signed-integer,
      (enums:timbre_assign(AssignMode)):2,
      (enums:onoff(EG2Reset)):1,
      (enums:onoff(EG1Reset)):1,
      (enums:timbre_trigger(TrigMode)):1, 0:1,
      (keypriority(KeyPrio)):2,
      UniDet:8,
      (pitch(Pitch)):4/bytes,
      (osc1(Osc1)):5/bytes,
      0:7, (enums:onoff(AudioIn1HPFGate)):1,
      0:8, 0:1, Porta:7,
      (vocoder_mixer(Mixer)):3/bytes,
      (vocoder_audioin1(AudioIn1)):3/bytes,
      (vocoder_filter(Filter)):6/bytes,
      (vocoder_amp(Amp)):5/bytes,
      0:8, 0:8, 127:8, 0:8,
      (eg(EG2)):4/bytes,
      (lfo(1, LFO1)):3/bytes,
      (lfo(2, LFO2)):3/bytes,
      (list_to_binary(ChLevels)):16/bytes,
      (panlevels(PanLevels)):16/bytes,
      0:(42*8)>>.

vocoder_mixer(#{osc1_lvl := Osc1Lvl,
		ext1_lvl := Ext1Lvl,
		noise := Noise}) ->
    <<Osc1Lvl:8, Ext1Lvl:8, Noise:8>>.

vocoder_audioin1(#{hpf_lvl := HPFLevel,
		   gate_sense := GateSense,
		   threshold := Threshold}) ->
    <<HPFLevel:8, GateSense:8, Threshold:8>>.

vocoder_filter(#{shift:=Shift, cutoff:=Cutoff, resonance:=Reso,
		 modsource:=ModSource, intensity:=Intesity,
		 efsense:=EFSense}) ->
    <<(vocoder_filter_shift(Shift)):8, (Cutoff+64):8, Reso:8,
      (enums:vocoder_filter_modsource(ModSource)):8, (Intesity+64):8,
      (vocoder_filter_efsense(EFSense)):8>>.

vocoder_filter_shift(0) -> 0;
vocoder_filter_shift(1) -> 1;
vocoder_filter_shift(2) -> 2;
vocoder_filter_shift(-1) -> 3;
vocoder_filter_shift(-2) -> 4.

vocoder_filter_efsense(hold) -> 127;
vocoder_filter_efsense(N) -> N.

vocoder_amp(#{level:=Lvl, direct_level:=DirectLvl, distortion:=Dist,
	      velocity_sense:=VelSense, key_track:=KeyTrack}) ->
    <<Lvl:8, DirectLvl:8, 0:7, (enums:onoff(Dist)):1,
      (VelSense + 64):8, (KeyTrack + 64):8>>.

panlevels(PanLevels) ->
    list_to_binary(lists:map(fun (X) -> X+64 end, PanLevels)).
