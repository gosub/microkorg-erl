-module(program_encode).
-export([from_map/1]).

from_map(#{name:=Name, arpctrl:=ArpCtrl, voice_mode:=VoiceMode,
	   scale_key:=ScaleKey, scale_type:=ScaleType, delayfx:=DelayFx,
	   modfx:=ModFx, eq:=Eq, arp:=Arp, kbd_oct:=KbdOctave,
	   voices:=Voices}) ->
    ArpCtrlData = arpctrl(ArpCtrl),
    VoiceModeData = enums:voice_mode(VoiceMode),
    <<Name:12/bytes, 0:16, ArpCtrlData:2/bytes, 0:2,
      VoiceModeData:2, 0:4>>.

%% to_map(ProgramData) ->
%%     <<Name:12/bytes, _:16, ArpCtrlData:2/bytes,
%%       _:2, VoiceMode:2, 0:4, ScaleKey:4, ScaleType:4, _:8,
%%       DelayFx:4/bytes, ModFx:3/bytes, Eq:4/bytes, Arp:7/bytes,
%%       KbdOctave/signed-integer, VoicesParams:216/bytes>> = ProgramData,
%%     Mode = enums:voice_mode(VoiceMode),
%%     #{name => Name,
%%       arpctrl => arpctrl(ArpCtrlData),
%%       voice_mode => Mode,
%%       scale_key => enums:scale_key(ScaleKey),
%%       scale_type => ScaleType,
%%       delayfx => delayfx(DelayFx),
%%       modfx => modfx(ModFx),
%%       eq => eq(Eq),
%%       arp => arp(Arp),
%%       kbd_oct => KbdOctave,
%%       voices => voices_to_list(Mode, VoicesParams)}.

arpctrl(#{len:=Len, pattern:=[T1,T2,T3,T4,T5,T6,T7,T8]}) ->
    LenDec = Len - 1,
    <<0:5, LenDec:3, T1:1,T2:1,T3:1,T4:1,T5:1,T6:1,T7:1,T8:1>>.
