-module(program_random).
-export([generate/0, generate/1, merge/2]).
-import(utils, [rnd/1]).

generate() ->
    generate_non_vocoder().

generate(true) ->
    generate_vocoder();
generate(false) ->
    generate_non_vocoder().

generate_non_vocoder() ->
    Mode = mode(),
    #{name => name(),
      arpctrl => arpctrl(),
      voice_mode => Mode,
      scale_key => 'C',
      scale_type => 0,
      delayfx => delayfx(),
      modfx => modfx(),
      eq => eq(),
      arp => arp(),
      kbd_oct => kbd_oct(),
      voices => voices(Mode)
     }.

generate_vocoder() ->
    #{name => name(),
      arpctrl => arpctrl(),
      voice_mode => vocoder,
      scale_key => 'C',
      scale_type => 0,
      delayfx => delayfx(),
      modfx => modfx(),
      eq => eq(),
      arp => arp(),
      kbd_oct => kbd_oct(),
      voices => [vocoder()]
     }.

% generic values generators

r01() -> rnd(2) - 1.

r127() -> rnd(128) - 1.

rrange(Min, Max) ->
    Min + rnd(Max-Min+1) - 1.

onoff() -> rnd({on, off}).

% funny name generator

name() ->
    name(rrange(2,4)).

name(2) ->
    <<(sil()):2/bytes, (sil()):2/bytes, "        ">>;
name(3) ->
    <<(sil()):2/bytes, (sil()):2/bytes, (sil()):2/bytes, "      ">>;
name(4) ->
    <<(sil()):2/bytes, (sil()):2/bytes,
      (sil()):2/bytes, (sil()):2/bytes, "    ">>.

sil() ->
    Voc = rnd({$a, $e, $i, $o, $u}),
    Con = rnd(list_to_tuple("bcdfglmnprstvz")),
    <<Con, Voc>>.

% end funny name generator

arpctrl() ->
    #{len => rnd(8),
      pattern => [r01(),r01(),r01(),r01(),
		  r01(),r01(),r01(),r01()]
     }.

% temporarly excluded 'vocoder' from possible modes
mode() ->
    rnd({single, double}).

delayfx() ->
    #{sync => onoff(),
      timebase => rnd(enums:values_of(delay_timebase)),
      time => r127(),
      depth => r127(),
      type => rnd(enums:values_of(delay_type))}.

modfx() ->
    #{lfo_speed => r127(),
      depth => r127(),
      type => rnd(enums:values_of(mod_type))}.

eq() ->
    #{hifreq => rnd(enums:values_of(hifreqs)),
      higain => gain(),
      lofreq => rnd(enums:values_of(lofreqs)),
      logain => gain()}.

gain() -> rrange(-12, 12).

arp() ->
    #{tempo => rrange(20, 300),
      onoff => onoff(),
      latch => onoff(),
      target => rnd(enums:values_of(arp_target)),
      keysync => onoff(),
      range => rrange(1,4),
      type => rnd(enums:values_of(arp_type)),
      gate_time => rrange(0, 100),
      resolution => rnd(enums:values_of(arp_reso)),
      swing => rrange(-100, 100)}.

kbd_oct() -> rrange(-3, 3).

voices(single) ->
    [timbre()];
voices(double) ->
    [timbre(), timbre()].

timbre() ->
    #{midi_ch => global,
      assign_mode => rnd(enums:values_of(timbre_assign)),
      eg2_reset => onoff(),
      eg1_reset => onoff(),
      trigger_mode => rnd(enums:values_of(timbre_trigger)),
      key_priority => last,
      unison_detune => rrange(0, 99),
      pitch => pitch(),
      osc1 => osc1(),
      osc2 => osc2(),
      porta_time => r127(),
      mixer => mixer(),
      filter => filter(),
      amp => amp(),
      eg1 => eg(),
      eg2 => eg(),
      lfo1 => lfo(1),
      lfo2 => lfo(2),
      patch => patch()
     }.

pitch() ->
    #{tune => rrange(-50, 50),
      bend => rrange(-12, 12),
      transpose => rrange(-24, 24),
      vibrato => rrange(-63, 63)}.

osc1() ->
    #{wave => rnd(enums:values_of(timbre1_wave)),
      ctrl1 => r127(),
      ctrl2 => r127(),
      dwgs => rnd(64)}.

osc2() ->
    #{modselect => rnd(enums:values_of(timbre2_modselect)),
      wave => rnd(enums:values_of(timbre2_wave)),
      semitone => rrange(-24, 24),
      tune => rrange(-63, 63)}.

mixer() ->
    #{osc1_lvl => r127(),
      osc2_lvl => r127(),
      noise => r127()}.

filter() ->
    #{type => rnd(enums:values_of(filter_type)),
      cutoff => r127(),
      reso => r127(),
      eg1_intensity => rrange(-63, 63),
      velocity_sense => rrange(-64,63),
      key_track => rrange(-63, 63)}.

amp() ->
    #{level => r127(),
      pan => rrange(-64,63),
      sw => r01(),
      distortion => onoff(),
      velocity_sense => rrange(-64,63),
      key_track => rrange(-63, 63)}.

eg() ->
    #{attack => r127(),
      decay => r127(),
      sustain => r127(),
      release => r127()}.

lfo(1) ->
    #{keysync => rnd(enums:values_of(lfo_keysync)),
      wave => rnd(enums:values_of(lfo1_wave)),
      freq => r127(),
      tempo_sync => onoff(),
      sync_note => rnd(enums:values_of(lfo_syncnote))};
lfo(2) ->
    #{keysync => rnd(enums:values_of(lfo_keysync)),
      wave => rnd(enums:values_of(lfo2_wave)),
      freq => r127(),
      tempo_sync => onoff(),
      sync_note => rnd(enums:values_of(lfo_syncnote))}.


patch() ->
    {patch_cable(), patch_cable(),
     patch_cable(), patch_cable()}.

patch_cable() ->
    #{destination => rnd(enums:values_of(cable_dest)),
      source => rnd(enums:values_of(cable_source)),
      intensity => rrange(-63, 63)}.

% vocoder patch specific generators

vocoder() ->
    #{midi_ch => global,
      assign_mode => rnd(enums:values_of(timbre_assign)),
      eg2_reset => onoff(),
      eg1_reset => onoff()}.

% random merging of two patches

merge(ProgramA, ProgramB) ->
    Merged = utils:deep_random_merge(ProgramA, ProgramB),
    constrain_voice_count(Merged).

constrain_voice_count(Program) ->
    #{voices := Voices, voice_mode := VoiceMode} = Program,
    constrain_voice_count(Program, VoiceMode, Voices).

constrain_voice_count(Program, single, [A, _]) ->
    maps:merge(Program, #{voices => [A]});
constrain_voice_count(Program, _, _) ->
    Program.
