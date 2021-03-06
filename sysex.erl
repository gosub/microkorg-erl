-module(sysex).
-export([encode/2, decode/1, encode_file/3, decode_file/1,
	request/1, request/2, request/3, function_id/1]).


trim(Sysex) ->
    Size = byte_size(Sysex)-6,
    <<16#F0, "B0X", Function:8, Payload:Size/bytes, 16#F7>> = Sysex,
    {function_id(Function), Payload}.


untrim(Payload, Function) ->
    <<16#F0, "B0X", (function_id(Function)):8, Payload/bytes, 16#F7>>.


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
    {Function, Data} = trim(SysexData),
    {Function, unscramble(Data)}.


decode_file(SysexFile) ->
    {ok, SysexData} = file:read_file(SysexFile),
    decode(SysexData).


encode(Function, Data) ->
    untrim(scramble(Data), Function).


encode_file(SysexFile, Function, Data) ->
    file:write_file(SysexFile, encode(Function, Data)).


% SYSTEM EXCLUSIVE FUNCTION IDs

function_id(16#40) -> current_program_data_dump;
function_id(16#4C) -> program_data_dump;
function_id(16#51) -> global_data_dump;
function_id(16#50) -> all_data_dump;
function_id(16#11) -> program_write_request;
function_id(current_program_data_dump) -> 16#40;
function_id(program_data_dump) -> 16#4C;
function_id(global_data_dump) -> 16#51;
function_id(all_data_dump) -> 16#50;
function_id(program_write_request) -> 16#11.


% SYSTEM EXCLUSIVE REQUEST MESSAGES

request(Request) ->
    request(Request, 0).

request(device_inquiry, MidiCh) -> <<16#F07E:16, MidiCh:8, 16#0601F7:24>>;
request(current_program_data_dump, MidiCh) -> generic_request(16#10, MidiCh);
request(program_data_dump, MidiCh) -> generic_request(16#1C, MidiCh);
request(global_data_dump, MidiCh) -> generic_request(16#0E, MidiCh);
request(all_data_dump, MidiCh) -> generic_request(16#0F, MidiCh).

% master volume
request(master_volume, Volume, any)
  when Volume >= 0, Volume =< 65535 ->
    <<16#F0, 16#7F, 16#7F, 16#04, 16#01, Volume:16/little, 16#F7>>;
request(master_volume, Volume, MidiCh)
  when Volume >= 0, Volume =< 65535 ->
    <<16#F0, 16#7F, MidiCh:8, 16#04, 16#01, Volume:16/little, 16#F7>>;
% master fine tune
request(master_fine_tune, FineTune, any)
  when FineTune >= 0, FineTune =< 32639 ->
    <<16#F0, 16#7F, 16#7F, 16#04, 16#03, FineTune:16/little, 16#F7>>;
request(master_fine_tune, FineTune, MidiCh)
  when FineTune >= 0, FineTune =< 32639 ->
    <<16#F0, 16#7F, MidiCh:8, 16#04, 16#03, FineTune:16/little, 16#F7>>.

generic_request(FunctionID, MidiCh) ->
    <<16#F042:16, (16#30 + MidiCh):8, 16#58, FunctionID, 16#F7>>.
