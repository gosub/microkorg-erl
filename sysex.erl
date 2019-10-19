-module(sysex).
-export([trim/1, untrim/1]).

trim(Sysex) ->
    PayloadSize = byte_size(Sysex)-6,
    <<16#F0,"B0X@",Payload:PayloadSize/bytes,16#F7>> = Sysex,
    Payload.

untrim(Payload) ->
    <<16#F0, "B0X@", Payload/bytes, 16#F7>>.
