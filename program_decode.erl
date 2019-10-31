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
   when GateTime =< 100, Swing >= -100, Swing =< 100 ->
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
voices_to_list(vocoder, <<Vocoder:104/bytes, _:112/bytes>>) ->
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
      pitch => timbre_pitch_to_map(Pitch),
      osc1 => timbre_osc1_to_map(Osc1),
      osc2 => timbre_osc2_to_map(Osc2),
      porta_time => PortamentoTime,
      mixer => timbre_mixer_to_map(Mixer),
      filter => timbre_filter_to_map(Filter),
      amp => timbre_amp_to_map(Amp),
      eg1 => timbre_eg_to_map(EG1),
      eg2 => timbre_eg_to_map(EG2),
      lfo1 => timbre_lfo_to_map(1, LFO1),
      lfo2 => timbre_lfo_to_map(2, LFO2),
      patch => timbre_patch_to_map(Patch)}.

timbre_midich(-1) -> global;
timbre_midich(N) when N >= 0 -> N.

timbre_keypriority(0) -> last;
timbre_keypriority(N) -> N.

timbre_pitch_to_map(<<Tune:8,Bend:8,Trans:8,Vibrato:8>>)
  when abs(Tune-64) =< 50, abs(Bend-64) =< 12,
       abs(Trans-64) =< 24, abs(Vibrato-64) =< 63 ->
    #{tune => Tune-64, bend => Bend-64,
     transpose => Trans-64, vibrato => Vibrato-64}.

timbre_osc1_to_map(<<Wave:8,WaveCtrl1:8,WaveCtrl2:8,DWGS:8,_:8>>) ->
    #{wave => timbre1_wave(Wave),
      ctrl1 => WaveCtrl1,
      ctrl2 => WaveCtrl2,
      dwgs => DWGS+1}.

timbre1_wave(0) -> saw;
timbre1_wave(1) -> pulse;
timbre1_wave(2) -> triangle;
timbre1_wave(3) -> sin;
timbre1_wave(4) -> vox;
timbre1_wave(5) -> dwgs;
timbre1_wave(6) -> noise;
timbre1_wave(7) -> audioin.

timbre_osc2_to_map(<<0:2,ModSelect:2,0:2,Wave:2,Semitone:8,Tune:8>>)
  when abs(Semitone-64) =< 24, abs(Tune-64) =< 63 ->
    #{modselect => timbre2_modselect(ModSelect),
     wave => timbre2_wave(Wave),
     semitone => Semitone-64,
     tune => Tune-64}.

timbre2_modselect(0) -> off;
timbre2_modselect(1) -> ring;
timbre2_modselect(2) -> sync;
timbre2_modselect(3) -> ringsync.

timbre2_wave(0) -> saw;
timbre2_wave(1) -> square;
timbre2_wave(2) -> triange.

timbre_mixer_to_map(<<Osc1Level:8, Osc2Level:8, Noise:8>>) ->
    #{osc1_lvl => Osc1Level,
      osc2_lvl => Osc2Level,
      noise => Noise}.

timbre_filter_to_map(<<Type:8,Cutoff:8,Reso:8,EG1Int:8,VelSens:8,KeyTrack:8>>)
  when abs(EG1Int-64) =< 63, abs(KeyTrack-64) =< 63 ->
    #{type => filter_type(Type),
     cutoff => Cutoff,
     reso => Reso,
     eg1_intensity => EG1Int-64,
     velocity_sense => VelSens-64,
     key_track => KeyTrack-64}.

filter_type(0) -> '24LPF';
filter_type(1) -> '12LPF';
filter_type(2) -> '12BPF';
filter_type(3) -> '12HPF'.

timbre_amp_to_map(<<Level:8,Pan:8,0:1,SW:1,0:5,Dist:1,
		    VelSense:8,KeyTrack:8>>) when abs(KeyTrack-64) =< 63 ->
    #{level => Level,
     pan => Pan-64,
     sw => SW,
     distortion => enums:onoff(Dist),
     velocity_sense => VelSense-64,
     key_track => KeyTrack-64}.

timbre_eg_to_map(<<Attack:8,Decay:8,Sustain:8,Release:8>>) ->
    #{attack => Attack, decay => Decay,
      sustain => Sustain, release => Release}.

timbre_lfo_to_map(N, <<0:2, KeySync:2, 0:2, Wave:2, Freq:8,
		       TempoSync:1, 0:2, SyncNote:5>>) ->
    #{keysync => lfo_keysync(KeySync),
      wave => lfo_wave(N, Wave),
      freq => Freq,
      tempo_sync => enums:onoff(TempoSync),
      sync_note => lfo_syncnote(SyncNote)}.

lfo_keysync(0) -> off;
lfo_keysync(1) -> timbre;
lfo_keysync(2) -> voice.

lfo_wave(_, 0) -> saw;
lfo_wave(_, 1) -> squ;
lfo_wave(1, 2) -> tri;
lfo_wave(2, 2) -> sin;
lfo_wave(_, 3) -> sh.

lfo_syncnote(N) ->
    lists:nth(N+1, ['1/1','3/4','2/3','1/2','3/8','1/3','1/4', '3/16',
		    '1/6','1/8','3/32','1/12','1/16','1/24','1/32']).

timbre_patch_to_map(<<Cable1:2/bytes, Cable2:2/bytes,
		      Cable3:2/bytes, Cable4:2/bytes>>) ->
    {patch_cable(Cable1), patch_cable(Cable2),
     patch_cable(Cable3), patch_cable(Cable4)}.

patch_cable(<<Destination:4, Source:4, Intensity:8>>)
  when abs(Intensity-64) =< 63 ->
    #{destination => patch_cable_dest(Destination),
      source => patch_cable_source(Source),
      intensity => Intensity - 64}.

patch_cable_dest(0) -> pitch;
patch_cable_dest(1) -> osc2pitch;
patch_cable_dest(2) -> osc1ctrl1;
patch_cable_dest(3) -> noise;
patch_cable_dest(4) -> cutoff;
patch_cable_dest(5) -> amp;
patch_cable_dest(6) -> pan;
patch_cable_dest(7) -> lfo2_freq.

patch_cable_source(0) -> eg1;
patch_cable_source(1) -> eg2;
patch_cable_source(2) -> lfo1;
patch_cable_source(3) -> lfo2;
patch_cable_source(4) -> velocity;
patch_cable_source(5) -> kbd_track;
patch_cable_source(6) -> pitch_bend;
patch_cable_source(7) -> mod.

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
      % TODO: complete decoding of remaining parameters
      pitch => Pitch,
      osc1 => Osc1,
      audioin1_hpfgate => AudioIn1HPFGate,
      porta_time => PortamentoTime,
      mixer => Mixer,
      audioin1 => AudioIn1,
      filter => Filter,
      amp => Amp,
      eg2 => EG2,
      lfo1 => LFO1,
      lfo2 => LFO2,
      ch_levels => ChLevels,
      pan_levels => PanLeves,
      hold_levels => HoldLevels}.
