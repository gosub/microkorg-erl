-module(program_random).
%-export([]).
-compile(export_all).

generate() ->
    #{name => name(),
      arpctrl => arpctrl()
      %voice_mode => mode(),
      %scale_key => scale(),
      %delayfx => delayfx(),
      %modfx => modfx(),
      %eq => eq(),
      %arp => arp(),
      %kbd_oct => kbd_oct(),
      %voices => voices()
     }.

rnd(N) when is_integer(N) ->
    rand:uniform(N);
rnd(T) when is_tuple(T) ->
    L = tuple_to_list(T),
    lists:nth(rnd(length(L)), L).


% TODO: funny name generator
name() ->
    <<"            ">>.
