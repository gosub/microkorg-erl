-module(program_decode).
-export([to_map/1]).

to_map(ProgramData) ->
    <<Name:12/bytes, _:16, ArpCtrlData:2/bytes,
      _:2, VoiceMode:2, 0:4, ScaleKey:4, ScaleType:4, _:8,
      DelayFx:4/bytes, ModFx:3/bytes, Eq:4/bytes, Arp:7/bytes,
      KbdOctave/signed-integer, VoicesParams:216/bytes>> = ProgramData,
    Mode = enums:voice_mode(VoiceMode),
    #{name => Name,
      arpctrl => arpctrl(ArpCtrlData),
      voice_mode => Mode,
      scale_key => enums:scale_key(ScaleKey),
      scale_type => ScaleType,
      delayfx => delayfx(DelayFx),
      modfx => modfx(ModFx),
      eq => eq(Eq),
      arp => arp(Arp),
      kbd_oct => KbdOctave,
      voices => voices_to_list(Mode, VoicesParams)}.

arpctrl(<<0:5, Len:3, T1:1,T2:1,T3:1,T4:1,T5:1,T6:1,T7:1,T8:1>>) ->
    #{len => Len+1, pattern => [T1,T2,T3,T4,T5,T6,T7,T8]}.

delayfx(<<Sync:1, 0:3, TimeBase:4,Time:8,Depth:8,Type:8>>) ->
    #{sync => enums:onoff(Sync),
      timebase => enums:delay_timebase(TimeBase),
      time => Time,
      depth => Depth,
      type => enums:delay_type(Type)}.

modfx(<<LFOSpeed:8,Depth:8,Type:8>>) ->
    #{lfo_speed => LFOSpeed,
     depth => Depth,
     type => enums:mod_type(Type)}.

eq(<<HiFreq:8,HiGain:8,LoFreq:8,LoGain:8>>) ->
    #{hifreq => enums:hifreqs(HiFreq),
     higain => gain(HiGain),
     lofreq => enums:lofreqs(LoFreq),
     logain => gain(LoGain)}.

gain(N) when N >= 64-12, N =< 64+12 -> N-64.

arp(<<Tempo:16, OnOff:1, Latch:1, Target:2, 0:3, KeySync:1,
      Range:4, Type:4, GateTime:8, Resolution:8, Swing/signed-integer>>)
   when Tempo >= 20, Tempo =< 300, GateTime =< 100,
	Swing >= -100, Swing =< 100 ->
    #{tempo => Tempo,
      onoff => enums:onoff(OnOff),
      latch => enums:onoff(Latch),
      target => enums:arp_target(Target),
      keysync => enums:onoff(KeySync),
      range => Range+1,
      type => enums:arp_type(Type),
      gate_time => GateTime,
      resolution => enums:arp_reso(Resolution),
      swing => Swing}.

voices_to_list(single, <<Timbre1:108/bytes, _:108/bytes>>) ->
    [timbre_to_map(Timbre1)];
voices_to_list(double, <<Timbre1:108/bytes, Timbre2:108/bytes>>) ->
    [timbre_to_map(Timbre1), timbre_to_map(Timbre2)];
voices_to_list(vocoder, <<Vocoder:142/bytes, _:74/bytes>>) ->
    [vocoder_to_map(Vocoder)].

timbre_to_map(<<MidiCh/signed-integer,
		AssignMode:2, EG2Reset:1, EG1Reset:1, TriggerMode:1,
	      _:1, KeyPriority:2, UnisonDetune:8, Pitch:4/bytes, Osc1:5/bytes,
	      Osc2:3/bytes, 0:1, PortamentoTime:7, Mixer:3/bytes,
	      Filter:6/bytes, Amp:5/bytes, EG1:4/bytes, EG2:4/bytes,
	      LFO1:3/bytes, LFO2:3/bytes, Patch:8/bytes, _:56/bytes>>)
  when UnisonDetune =< 99 ->
    #{midi_ch => timbre_midich(MidiCh),
      assign_mode => enums:timbre_assign(AssignMode),
      eg2_reset => enums:onoff(EG2Reset),
      eg1_reset => enums:onoff(EG1Reset),
      trigger_mode => enums:timbre_trigger(TriggerMode),
      key_priority => timbre_keypriority(KeyPriority),
      unison_detune => UnisonDetune,
      pitch => timbre_pitch(Pitch),
      osc1 => timbre_osc1(Osc1),
      osc2 => timbre_osc2(Osc2),
      porta_time => PortamentoTime,
      mixer => timbre_mixer(Mixer),
      filter => timbre_filter(Filter),
      amp => timbre_amp(Amp),
      eg1 => timbre_eg(EG1),
      eg2 => timbre_eg(EG2),
      lfo1 => timbre_lfo(1, LFO1),
      lfo2 => timbre_lfo(2, LFO2),
      patch => timbre_patch(Patch)}.

timbre_midich(-1) -> global;
timbre_midich(N) when N >= 0 -> N.

timbre_keypriority(0) -> last;
timbre_keypriority(N) -> N.

timbre_pitch(<<Tune:8,Bend:8,Trans:8,Vibrato:8>>)
  when abs(Tune-64) =< 50, abs(Bend-64) =< 12,
       abs(Trans-64) =< 24, abs(Vibrato-64) =< 63 ->
    #{tune => Tune-64, bend => Bend-64,
     transpose => Trans-64, vibrato => Vibrato-64}.

timbre_osc1(<<Wave:8,WaveCtrl1:8,WaveCtrl2:8,DWGS:8,_:8>>) ->
    #{wave => enums:timbre1_wave(Wave),
      ctrl1 => WaveCtrl1,
      ctrl2 => WaveCtrl2,
      dwgs => DWGS+1}.

timbre_osc2(<<0:2,ModSelect:2,0:2,Wave:2,Semitone:8,Tune:8>>)
  when abs(Semitone-64) =< 24, abs(Tune-64) =< 63 ->
    #{modselect => enums:timbre2_modselect(ModSelect),
     wave => enums:timbre2_wave(Wave),
     semitone => Semitone-64,
     tune => Tune-64}.

timbre_mixer(<<Osc1Level:8, Osc2Level:8, Noise:8>>) ->
    #{osc1_lvl => Osc1Level,
      osc2_lvl => Osc2Level,
      noise => Noise}.

timbre_filter(<<Type:8,Cutoff:8,Reso:8,EG1Int:8,VelSens:8,KeyTrack:8>>)
  when abs(EG1Int-64) =< 63, abs(KeyTrack-64) =< 63 ->
    #{type => enums:filter_type(Type),
     cutoff => Cutoff,
     reso => Reso,
     eg1_intensity => EG1Int-64,
     velocity_sense => VelSens-64,
     key_track => KeyTrack-64}.

timbre_amp(<<Level:8,Pan:8,0:1,SW:1,0:5,Dist:1,VelSense:8,KeyTrack:8>>)
  when abs(KeyTrack-64) =< 63 ->
    #{level => Level,
     pan => Pan-64,
     sw => SW,
     distortion => enums:onoff(Dist),
     velocity_sense => VelSense-64,
     key_track => KeyTrack-64}.

timbre_eg(<<Attack:8,Decay:8,Sustain:8,Release:8>>) ->
    #{attack => Attack, decay => Decay,
      sustain => Sustain, release => Release}.

timbre_lfo(N, <<0:2, KeySync:2, 0:2, Wave:2, Freq:8,
		TempoSync:1, 0:2, SyncNote:5>>) ->
    #{keysync => enums:lfo_keysync(KeySync),
      wave => enums:lfo_wave(N, Wave),
      freq => Freq,
      tempo_sync => enums:onoff(TempoSync),
      sync_note => enums:lfo_syncnote(SyncNote)}.

timbre_patch(<<Cable1:2/bytes, Cable2:2/bytes,
	       Cable3:2/bytes, Cable4:2/bytes>>) ->
    {patch_cable(Cable1), patch_cable(Cable2),
     patch_cable(Cable3), patch_cable(Cable4)}.

patch_cable(<<Destination:4, Source:4, Intensity:8>>)
  when abs(Intensity-64) =< 63 ->
    #{destination => enums:cable_dest(Destination),
      source => enums:cable_source(Source),
      intensity => Intensity - 64}.

vocoder_to_map(<<MidiCh/signed-integer,
		 AssignMode:2, EG2Reset:1, EG1Reset:1, TriggerMode:1,
		 _:1, KeyPriority:2, UnisonDetune:8, Pitch:4/bytes, Osc1:5/bytes,
		 _:7, AudioIn1HPFGate:1, _:8, 0:1, PortamentoTime:7, 
		 Mixer:3/bytes, AudioIn1:3/bytes, Filter:6/bytes,
		 Amp:5/bytes, 0:8, 0:8, 127:8, 0:8, EG2:4/bytes,
		 LFO1:3/bytes, LFO2:3/bytes, ChLevels:16/bytes,
		 PanLeves:16/bytes, HoldLevels:64/bytes>>)
  when UnisonDetune =< 99 ->
    #{midi_ch => timbre_midich(MidiCh),
      assign_mode => enums:timbre_assign(AssignMode),
      eg2_reset => enums:onoff(EG2Reset),
      eg1_reset => enums:onoff(EG1Reset),
      trigger_mode => enums:timbre_trigger(TriggerMode),
      key_priority => timbre_keypriority(KeyPriority),
      unison_detune => UnisonDetune,
      pitch => timbre_pitch(Pitch),
      osc1 => timbre_osc1(Osc1),
      audioin1_hpfgate => enums:onoff(AudioIn1HPFGate),
      porta_time => PortamentoTime,
      mixer => vocoder_mixer(Mixer),
      audioin1 => vocoder_audioin1(AudioIn1),
      % TODO: complete decoding of remaining parameters
      filter => Filter,
      amp => Amp,
      eg2 => EG2,
      lfo1 => LFO1,
      lfo2 => LFO2,
      ch_levels => ChLevels,
      pan_levels => PanLeves,
      hold_levels => HoldLevels}.

vocoder_mixer(<<Osc1Level:8, Ext1Level:8, Noise:8>>) ->
    #{osc1_lvl => Osc1Level,
      ext1_lvl => Ext1Level,
      noise => Noise}.

vocoder_audioin1(<<HPFLevel:8, GateSense:8, Threshold:8>>) ->
    #{hpf_lvl => HPFLevel,
      gate_sense => GateSense,
      threshold => Threshold}.
