-module(sysex).
-export([trim/1, untrim/1, unscramble/1]).

trim(Sysex) ->
    PayloadSize = byte_size(Sysex)-6,
    <<16#F0,"B0X@",Payload:PayloadSize/bytes,16#F7>> = Sysex,
    Payload.

untrim(Payload) ->
    <<16#F0, "B0X@", Payload/bytes, 16#F7>>.

unscramble(<<>>) -> <<>>;

unscramble(<<0:1,X7:1,X6:1,X5:1,X4:1,X3:1,X2:1,X1:1,
	     B1:8,B2:8,B3:8,B4:8,B5:8,B6:8,B7:8,
	     Rest/binary>>) ->
    Tail = unscramble(Rest),
    <<(B1 bor (X1 bsl 7)):8, (B2 bor (X2 bsl 7)):8, (B3 bor (X3 bsl 7)):8,
      (B4 bor (X4 bsl 7)):8, (B5 bor (X5 bsl 7)):8, (B6 bor (X6 bsl 7)):8,
      (B7 bor (X7 bsl 7)):8, Tail/binary>>;

unscramble(<<0:2,X6:1,X5:1,X4:1,X3:1,X2:1,X1:1,
	     B1:8,B2:8,B3:8,B4:8,B5:8,B6:8>>) ->
    <<(B1 bor (X1 bsl 7)):8, (B2 bor (X2 bsl 7)):8, (B3 bor (X3 bsl 7)):8,
      (B4 bor (X4 bsl 7)):8, (B5 bor (X5 bsl 7)):8, (B6 bor (X6 bsl 7)):8>>;

unscramble(<<0:3,X5:1,X4:1,X3:1,X2:1,X1:1,
	     B1:8,B2:8,B3:8,B4:8,B5:8>>) ->
    <<(B1 bor (X1 bsl 7)):8, (B2 bor (X2 bsl 7)):8, (B3 bor (X3 bsl 7)):8,
      (B4 bor (X4 bsl 7)):8, (B5 bor (X5 bsl 7)):8>>;

unscramble(<<0:4,X4:1,X3:1,X2:1,X1:1,B1:8,B2:8,B3:8,B4:8>>) ->
    <<(B1 bor (X1 bsl 7)):8, (B2 bor (X2 bsl 7)):8, (B3 bor (X3 bsl 7)):8,
      (B4 bor (X4 bsl 7)):8>>;

unscramble(<<0:5,X3:1,X2:1,X1:1,B1:8,B2:8,B3:8>>) ->
    <<(B1 bor (X1 bsl 7)):8, (B2 bor (X2 bsl 7)):8, (B3 bor (X3 bsl 7)):8>>;

unscramble(<<0:6,X2:1,X1:1,B1:8,B2:8>>) ->
    <<(B1 bor (X1 bsl 7)):8, (B2 bor (X2 bsl 7)):8>>;

unscramble(<<0:7,X1:1,B1:8>>) ->
    <<(B1 bor (X1 bsl 7)):8>>.


scramble(<<>>) -> <<>>;

scramble(<<X1:1,B1:7,X2:1,B2:7,X3:1,B3:7,X4:1,B4:7,
	   X5:1,B5:7,X6:1,B6:7,X7:1,B7:7,Rest/binary>>) ->
    Tail = scramble(Rest),
    <<0:1,X7:1,X6:1,X5:1,X4:1,X3:1,X2:1,X1:1,
      B1:8,B2:8,B3:8,B4:8,B5:8,B6:8,B7:8,Tail/binary>>;

scramble(<<X1:1,B1:7,X2:1,B2:7,X3:1,B3:7,
	   X4:1,B4:7,X5:1,B5:7,X6:1,B6:7>>) ->
    <<0:2,X6:1,X5:1,X4:1,X3:1,X2:1,X1:1,
      B1:8,B2:8,B3:8,B4:8,B5:8,B6:8>>;

scramble(<<X1:1,B1:7,X2:1,B2:7,X3:1,B3:7,
	   X4:1,B4:7,X5:1,B5:7>>) ->
    <<0:3,X5:1,X4:1,X3:1,X2:1,X1:1,
      B1:8,B2:8,B3:8,B4:8,B5:8>>;

scramble(<<X1:1,B1:7,X2:1,B2:7,X3:1,B3:7,X4:1,B4:7>>) ->
    <<0:4,X4:1,X3:1,X2:1,X1:1,B1:8,B2:8,B3:8,B4:8>>;

scramble(<<X1:1,B1:7,X2:1,B2:7,X3:1,B3:7>>) ->
    <<0:5,X3:1,X2:1,X1:1,B1:8,B2:8,B3:8>>;

scramble(<<X1:1,B1:7,X2:1,B2:7>>) ->
    <<0:6,X2:1,X1:1,B1:8,B2:8>>;

scramble(<<X1:1,B1:7>>) ->
    <<0:7,X1:1,B1:8>>.

