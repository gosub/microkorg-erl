-module(enums).
-export([onoff/1, voice_mode/1, scale_key/1, delay_timebase/1, delay_type/1,
	 mod_type/1, hifreqs/1, lofreqs/1, arp_target/1, arp_type/1,
	 arp_reso/1]).

-define(SCALE_KEY, ['C','C#','D','D#','E','F',
		    'F#','G','G#','A','A#','B']).
-define(DLY_TIMEBASE, ['1/32','1/24','1/16','1/12','3/32','1/8','1/6',
		       '3/16','1/4','1/3','3/8','1/2','2/3','3/4','1/1']).
-define(HI_FREQS, [1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000,
		   3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 5250,
		   5500, 5750, 6000, 7000, 8000, 9000, 10000, 11000, 12000,
		   14000, 16000, 18000]).

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

hifreqs(N) when N >= 0, N =< 29 -> lists:nth(N+1, ?HI_FREQS);
hifreqs(N) when N >= 1000, N =< 18000 -> utils:find(N, ?HI_FREQS).

