-module(enums).
-export([onoff/1, voice_mode/1, scale_key/1, delay_timebase/1, delay_type/1,
	 mod_type/1, hifreqs/1, lofreqs/1, arp_target/1, arp_type/1,
	 arp_reso/1, timbre_assign/1, timbre_trigger/1, timbre1_wave/1,
	 timbre2_modselect/1, timbre2_wave/1, filter_type/1, lfo_keysync/1,
	 lfo_wave/2, lfo_syncnote/1, cable_dest/1, cable_source/1, values_of/1]).

-define(SCALE_KEY, ['C','C#','D','D#','E','F',
		    'F#','G','G#','A','A#','B']).
-define(DLY_TIMEBASE, ['1/32','1/24','1/16','1/12','3/32','1/8','1/6',
		       '3/16','1/4','1/3','3/8','1/2','2/3','3/4','1/1']).
-define(HI_FREQS, [1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000,
		   3250, 3500, 3750, 4000, 4250, 4500, 4750, 5000, 5250,
		   5500, 5750, 6000, 7000, 8000, 9000, 10000, 11000, 12000,
		   14000, 16000, 18000]).
-define(LO_FREQS, [40, 50, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240,
		   260, 280, 300, 320, 340, 360, 380, 400, 420, 440, 460,
		   480, 500, 600, 700, 800, 900, 1000]).
-define(LFO_SYNCNOTE, ['1/1','3/4','2/3','1/2','3/8','1/3','1/4', '3/16',
		    '1/6','1/8','3/32','1/12','1/16','1/24','1/32']).

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
hifreqs(N) when N >= 1000, N =< 18000 ->
    {ok, Pos} = utils:find(N, ?HI_FREQS),
    Pos.

lofreqs(N) when N >= 0, N =< 29 -> lists:nth(N+1, ?LO_FREQS);
lofreqs(N) when N >= 40, N =< 1000 ->
    {ok, Pos} = utils:find(N, ?LO_FREQS),
    Pos.

arp_target(0) -> both;
arp_target(1) -> timbre1;
arp_target(2) -> timbre2;
arp_target(both)    -> 0;
arp_target(timbre1) -> 1;
arp_target(timbre2) -> 2.

arp_type(0) -> up;
arp_type(1) -> down;
arp_type(2) -> alt1;
arp_type(3) -> alt2;
arp_type(4) -> random;
arp_type(5) -> trigger;
arp_type(up)      -> 0;
arp_type(down)    -> 1;
arp_type(alt1)    -> 2;
arp_type(alt2)    -> 3;
arp_type(random)  -> 4;
arp_type(trigger) -> 5.

arp_reso(0) -> '1/24';
arp_reso(1) -> '1/16';
arp_reso(2) -> '1/12';
arp_reso(3) -> '1/8';
arp_reso(4) -> '1/6';
arp_reso(5) -> '1/4';
arp_reso('1/24') -> 0;
arp_reso('1/16') -> 1;
arp_reso('1/12') -> 2;
arp_reso('1/8')  -> 3;
arp_reso('1/6')  -> 4;
arp_reso('1/4')  -> 5.

timbre_assign(0) -> mono;
timbre_assign(1) -> poly;
timbre_assign(2) -> unison;
timbre_assign(mono)   -> 0;
timbre_assign(poly)   -> 1;
timbre_assign(unison) -> 2.

timbre_trigger(0) -> single;
timbre_trigger(1) -> multi;
timbre_trigger(single) -> 0;
timbre_trigger(multi)  -> 1.

timbre1_wave(0) -> saw;
timbre1_wave(1) -> pulse;
timbre1_wave(2) -> triangle;
timbre1_wave(3) -> sin;
timbre1_wave(4) -> vox;
timbre1_wave(5) -> dwgs;
timbre1_wave(6) -> noise;
timbre1_wave(7) -> audioin;
timbre1_wave(saw)      -> 0;
timbre1_wave(pulse)    -> 1;
timbre1_wave(triangle) -> 2;
timbre1_wave(sin)      -> 3;
timbre1_wave(vox)      -> 4;
timbre1_wave(dwgs)     -> 5;
timbre1_wave(noise)    -> 6;
timbre1_wave(audioin)  -> 7.

timbre2_modselect(0) -> off;
timbre2_modselect(1) -> ring;
timbre2_modselect(2) -> sync;
timbre2_modselect(3) -> ringsync;
timbre2_modselect(off)      -> 0;
timbre2_modselect(ring)     -> 1;
timbre2_modselect(sync)     -> 2;
timbre2_modselect(ringsync) -> 3.

timbre2_wave(0) -> saw;
timbre2_wave(1) -> square;
timbre2_wave(2) -> triange;
timbre2_wave(saw)     -> 0;
timbre2_wave(square)  -> 1;
timbre2_wave(triange) -> 2.

filter_type(0) -> '24LPF';
filter_type(1) -> '12LPF';
filter_type(2) -> '12BPF';
filter_type(3) -> '12HPF';
filter_type('24LPF') -> 0;
filter_type('12LPF') -> 1;
filter_type('12BPF') -> 2;
filter_type('12HPF') -> 3.

lfo_keysync(0) -> off;
lfo_keysync(1) -> timbre;
lfo_keysync(2) -> voice;
lfo_keysync(off)    -> 0;
lfo_keysync(timbre) -> 1;
lfo_keysync(voice)  -> 2.

lfo_wave(_, 0) -> saw;
lfo_wave(_, 1) -> squ;
lfo_wave(1, 2) -> tri;
lfo_wave(2, 2) -> sin;
lfo_wave(_, 3) -> sh;
lfo_wave(_, saw) -> 0;
lfo_wave(_, squ) -> 1;
lfo_wave(1, tri) -> 2;
lfo_wave(2, sin) -> 2;
lfo_wave(_, sh)  -> 3.

lfo_syncnote(X) ->
    F = utils:list_to_fun(?LFO_SYNCNOTE),
    F(X).

cable_dest(0) -> pitch;
cable_dest(1) -> osc2pitch;
cable_dest(2) -> osc1ctrl1;
cable_dest(3) -> noise;
cable_dest(4) -> cutoff;
cable_dest(5) -> amp;
cable_dest(6) -> pan;
cable_dest(7) -> lfo2_freq;
cable_dest(pitch)     -> 0;
cable_dest(osc2pitch) -> 1;
cable_dest(osc1ctrl1) -> 2;
cable_dest(noise)     -> 3;
cable_dest(cutoff)    -> 4;
cable_dest(amp)       -> 5;
cable_dest(pan)       -> 6;
cable_dest(lfo2_freq) -> 7.

cable_source(0) -> eg1;
cable_source(1) -> eg2;
cable_source(2) -> lfo1;
cable_source(3) -> lfo2;
cable_source(4) -> velocity;
cable_source(5) -> kbd_track;
cable_source(6) -> pitch_bend;
cable_source(7) -> mod;
cable_source(eg1)        -> 0;
cable_source(eg2)        -> 1;
cable_source(lfo1)       -> 2;
cable_source(lfo2)       -> 3;
cable_source(velocity)   -> 4;
cable_source(kbd_track)  -> 5;
cable_source(pitch_bend) -> 6;
cable_source(mod)        -> 7.

values_of(delay_timebase) -> list_to_tuple(?DLY_TIMEBASE);
values_of(delay_type) -> {stereo, cross, lr}.
