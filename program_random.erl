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
    #{}.
