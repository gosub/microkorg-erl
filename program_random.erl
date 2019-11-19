-module(program_random).
%-export([]).
-compile(export_all).

generate() ->
    Mode = mode(),
    #{name => name(),
      arpctrl => arpctrl(),
      voice_mode => Mode,
      scale_key => 'C',
      delayfx => delayfx(),
      modfx => modfx(),
      eq => eq(),
      arp => arp(),
      kbd_oct => kbd_oct(),
      voices => voices(Mode)
     }.

rnd(N) when is_integer(N) ->
    rand:uniform(N);
rnd(T) when is_tuple(T) ->
    element(rnd(tuple_size(T)), T).

r01() -> rnd(2) - 1.

r127() -> rnd(128) - 1.

r255() -> rnd(256) - 1.

rrange(Min, Max) ->
    Min + rnd(Max-Min+1) - 1.

onoff() -> rnd({on, off}).

% TODO: funny name generator
name() ->
    <<"            ">>.

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
      time => r255(),
      depth => r255(),
      type => rnd(enums:values_of(delay_type))}.

modfx() ->
    #{lfo_speed => r255(),
      depth => r255(),
      type => rnd(enums:values_of(mod_type))}.

eq() ->
    #{hifreq => rnd(enums:values_of(hifreqs)),
      higain => gain(),
      lofreq => rnd(enums:values_of(lofreqs)),
      logain => gain()}.

gain() -> rrange(-12, 12).

arp() ->
    #{tempo => rrange(0, 65535), % 16bit range
      onoff => onoff(),
      latch => onoff(),
      target => rnd(enums:values_of(arp_target)),
      range => rrange(1,16),
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
      eg2 => eg()
     }.

pitch() ->
    #{tune => rrange(-50, 50),
      bend => rrange(-12, 12),
      transpose => rrange(-24, 24),
      vibrato => rrange(-63, 63)}.

osc1() ->
    #{wave => rnd(enums:values_of(timbre1_wave)),
      ctrl1 => r255(),
      ctrl2 => r255(),
      dwgs => rnd(128)}.

osc2() ->
    #{modselect => rnd(enums:values_of(timbre2_modselect)),
      wave => rnd(enums:values_of(timbre2_wave)),
      semitone => rrange(-24, 24),
      tune => rrange(-63, 63)}.

mixer() ->
    #{osc1_lvl => r255(),
      osc2_lvl => r255(),
      noise => r255()}.

filter() ->
    #{type => rnd(enums:values_of(filter_type)),
      cutoff => r255(),
      reso => r255(),
      eg1_intensity => rrange(-63, 63),
      velocity_sense => rrange(-64,64),
      key_track => rrange(-63, 63)}.

amp() ->
    #{level => r255(),
      pan => rrange(-64,64),
      sw => r01(),
      distortion => onoff(),
      velocity_sense => rrange(-64,64),
      key_track => rrange(-63, 63)}.

eg() ->
    #{attack => r255(),
      decay => r255(),
      sustain => r255(),
      release => r255()}.
