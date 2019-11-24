-module(sysex).
-export([trim/1, untrim/1, scramble/1, unscramble/1,
	encode/1, decode/1, encode_file/2, decode_file/1,
	request/1, request/2]).


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


decode(SysexData) ->
    unscramble(trim(SysexData)).


decode_file(SysexFile) ->
    {ok, SysexData} = file:read_file(SysexFile),
    decode(SysexData).


encode(Data) ->
    untrim(scramble(Data)).


encode_file(SysexFile, Data) ->
    file:write_file(SysexFile, encode(Data)).


% MIDI REQUEST MESSAGES

request(Request) ->
    request(Request, 0).

request(device_inquiry, MidiCh) -> <<16#F07E:16, MidiCh:8, 16#0601F7:24>>;
request(current_program_data_dump, MidiCh) -> generic_request(16#10, MidiCh);
request(program_data_dump, MidiCh) -> generic_request(16#1C, MidiCh);
request(global_data_dump, MidiCh) -> generic_request(16#0E, MidiCh);
request(all_data_dump, MidiCh) -> generic_request(16#0F, MidiCh).

generic_request(FunctionID, MidiCh) ->
    <<16#F042:16, (16#30 + MidiCh):8, 16#58, FunctionID, 16#F7>>.
