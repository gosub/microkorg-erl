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

arp_to_map(<<Tempo:16, OnOff:1, Latch:1, Target:2, 0:3, KeySync:1,
	     Range:4, Type:4, GateTime:8, Resolution:8, Swing/signed-integer>>)
   when GateTime =< 100, Swing >= -100, Swing =< 100 ->
    #{tempo => Tempo,
      onoff => bin2onoff(OnOff),
      latch => bin2onoff(Latch),
      target => arp_target_from_int(Target),
      keysync => bin2onoff(KeySync),
      range => Range+1,
      type => arp_type_from_int(Type),
      gate_time => GateTime,
      resolution => arp_reso_from_int(Resolution),
      swing => Swing}.

arp_target_from_int(0) -> both;
arp_target_from_int(1) -> timbre1;
arp_target_from_int(2) -> timbre2.

arp_type_from_int(0) -> up;
arp_type_from_int(1) -> down;
arp_type_from_int(2) -> alt1;
arp_type_from_int(3) -> alt2;
arp_type_from_int(4) -> random;
arp_type_from_int(5) -> trigger.

arp_reso_from_int(0) -> '1/24';
arp_reso_from_int(1) -> '1/16';
arp_reso_from_int(2) -> '1/12';
arp_reso_from_int(3) -> '1/8';
arp_reso_from_int(4) -> '1/6';
arp_reso_from_int(5) -> '1/4'.

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
    #{midi_ch => timbre_midich_from_int(MidiCh),
      assign_mode => timbre_assign_from_int(AssignMode),
      eg2_reset => bin2onoff(EG2Reset),
      eg1_reset => bin2onoff(EG1Reset),
      trigger_mode => timbre_trigger_from_int(TriggerMode),
      key_priority => timbre_keypriority_from_int(KeyPriority),
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

timbre_midich_from_int(-1) -> global;
timbre_midich_from_int(N) when N >= 0 -> N.

timbre_assign_from_int(0) -> mono;
timbre_assign_from_int(1) -> poly;
timbre_assign_from_int(2) -> unison.

timbre_trigger_from_int(0) -> single;
timbre_trigger_from_int(1) -> multi.

timbre_keypriority_from_int(0) -> last;
timbre_keypriority_from_int(N) -> N.

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

timbre_eg_to_map(TODO) ->
    TODO.

timbre_lfo_to_map(N, TODO) ->
    TODO.
timbre_amp_to_map(<<Level:8,Pan:8,0:1,SW:1,0:5,Dist:1,
		    VelSense:8,KeyTrack:8>>) when abs(KeyTrack-64) =< 63 ->
    #{level => Level,
     pan => Pan-64,
     sw => SW,
     distortion => bin2onoff(Dist),
     velocity_sense => VelSense-64,
     key_track => KeyTrack-64}.

timbre_patch_to_map(TODO) ->
    TODO.

vocoder_to_map(TODO) ->
    TODO.
