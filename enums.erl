-module(enums).
-export([onoff/1, voice_mode/1, scale_key/1]).

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

scale_key(0) -> 'C';
scale_key(1) -> 'C#';
scale_key(2) -> 'D';
scale_key(3) -> 'D#';
scale_key(4) -> 'E';
scale_key(5) -> 'F';
scale_key(6) -> 'F#';
scale_key(7) -> 'G';
scale_key(8) -> 'G#';
scale_key(9) -> 'A';
scale_key(10) -> 'A#';
scale_key(11) -> 'B';
scale_key('C')  -> 0;
scale_key('C#') -> 1;
scale_key('D')  -> 2;
scale_key('D#') -> 3;
scale_key('E')  -> 4;
scale_key('F')  -> 5;
scale_key('F#') -> 6;
scale_key('G')  -> 7;
scale_key('G#') -> 8;
scale_key('A')  -> 9;
scale_key('A#') -> 10;
scale_key('B')  -> 11.
