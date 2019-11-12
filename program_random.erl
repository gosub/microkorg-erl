-module(program_random).
-export([]).

generate() ->
    #{name => name(),
      arpctrl => arpctrl(),
      voice_mode => mode(),
      scale_key => scale(),
      delayfx => delayfx(),
      modfx => modfx(),
      eq => eq(),
      arp => arp(),
      kbd_oct => kbd_oct(),
      voices => voices()}.

% TODO: funny name generator
name() ->
    <<"            ">>.
