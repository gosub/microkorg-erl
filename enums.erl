-module(enums).
-export([onoff/1, voice_mode/1, scale_key/1]).

-define(SCALE_KEY, ['C','C#','D','D#','E','F',
		    'F#','G','G#','A','A#','B']).


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
