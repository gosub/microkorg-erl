-module(program_random).
%-export([]).
-compile(export_all).

generate() ->
    Mode = mode(),
    #{name => name(),
      arpctrl => arpctrl(),
      voice_mode => Mode,
      scale_key => enums:scale_key(0)
      %delayfx => delayfx(),
      %modfx => modfx(),
      %eq => eq(),
      %arp => arp(),
      %kbd_oct => kbd_oct(),
      %voices => voices(Mode)
     }.

rnd(N) when is_integer(N) ->
    rand:uniform(N);
rnd(T) when is_tuple(T) ->
    element(rnd(tuple_size(T)), T).

r01() -> rnd(2) - 1.

r127() -> rnd(128) - 1.

onoff() -> rnd({on, off}).

% TODO: funny name generator
name() ->
    <<"            ">>.

arpctrl() ->
    #{len => rnd(8),
      pattern => [r01(),r01(),r01(),r01(),
		  r01(),r01(),r01(),r01()]
     }.

mode() ->
    rnd({single, double}).
