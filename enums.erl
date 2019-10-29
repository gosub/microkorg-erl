-module(enums).
-export([onoff/1, voice_mode/1, scale_key/1, delay_timebase/1, delay_type/1,
	 mod_type/1]).

-define(SCALE_KEY, ['C','C#','D','D#','E','F',
		    'F#','G','G#','A','A#','B']).
-define(DLY_TIMEBASE, ['1/32','1/24','1/16','1/12','3/32','1/8','1/6',
		       '3/16','1/4','1/3','3/8','1/2','2/3','3/4','1/1']).


onoff(0) -> off;
onoff(1) -> on;
onoff(off) -> 0;
onoff(on)  -> 1.

voice_mode(0) -> single;
voice_mode(2) -> double;
voice_mode(3) -> vocoder;
voice_mode(single)  -> 0;
voice_mode(double)  -> 2;
voice_mode(vocoder) -> 3.

scale_key(X) ->
    F = utils:list_to_fun(?SCALE_KEY),
    F(X).

delay_timebase(X) ->
    F = utils:list_to_fun(?DLY_TIMEBASE),
    F(X).

delay_type(0) -> stereo;
delay_type(1) -> cross;
delay_type(2) -> lr;
delay_type(stereo) -> 0;
delay_type(cross)  -> 1;
delay_type(lr)     -> 2.

mod_type(0) -> chorus;
mod_type(1) -> ensemble;
mod_type(2) -> phaser;
mod_type(chorus)   -> 0;
mod_type(ensemble) -> 1;
mod_type(phaser)   -> 2.
