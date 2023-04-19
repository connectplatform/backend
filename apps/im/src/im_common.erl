-module(im_common).

-include("im_common.hrl").

-define(CHARS, "abcdefghijklmnopqrstuvwxyz").
-define(NUMBERS, "1234567890").

-export([format_id/1, parse_id/1]).
-export([format_integer/1]).
-export([format_utf8/1, length_utf8/1, format_utf8_array/1, crop_str/2, strip/1, trim/1, list_unique/1]).
-export([format_phone/1, format_phone_with_code/2, format_lookup_value/1]).
-export([format_price/2]).
-export([ensure_timestamp/1, ensure_list/1, ensure_binary/1, ensure_float/1, ensure_integer/1, ensure_boolean/1, binary_to_num/1]).
-export([parse_boolean/1]).
-export([get_county_code/1, get_national_number/1, is_valid_phone/1]).
-export([get_binary_length/1, binarize_proplist/1]).
-export([proplist_to_tuple/1, tuple_to_proplist/1]).
-export([read_json_file/1, json_to_mongo/1, mongo_to_json/1, read_lines/1, mongo_tuple_to_proplist/1]).
-export([crypto_random_string/0, crypto_random_string/2]).
-export([random_number/2, random_string/2]).
-export([timestamp_to_datetime/1, datetime_to_timestamp/1]).
-export([base_url/0]).
-export([proplist_upsert/3]).

format_id(undefined)             -> undefined;
format_id(<<>>)                  -> undefined;
format_id({ObjectId})            -> ensure_binary(sm:bin_to_hex(ObjectId));
format_id(Id) when is_list(Id)   -> ensure_binary(Id);
format_id(Id) when is_binary(Id) -> Id.

parse_id(undefined) -> undefined;
parse_id(<<>>)      -> undefined;
parse_id({Binary})  -> {Binary};
parse_id(Id)        -> try {sm:hex_to_bin(Id)} catch _:_ -> undefined end.

format_utf8(undefined) -> undefined;
format_utf8(null) -> undefined;
format_utf8(Value) when is_binary(Value) -> Value;
format_utf8(Value) when is_list(Value) ->
  case io_lib:printable_list(Value) of
    true -> unicode:characters_to_binary(Value);
    false ->
      case is_integer(hd(Value)) of
        true  -> unicode:characters_to_binary(Value);
        false -> [format_utf8(V) || V <- Value]
      end
  end;
format_utf8(Value) when is_integer(Value) -> format_utf8(integer_to_list(Value));
format_utf8(_) -> <<>>.

format_utf8_array(undefined) -> [];
format_utf8_array(Array) ->
  [format_utf8(Item) || Item <- Array].

format_integer(Value) when is_integer(Value) -> Value;
format_integer(_) -> undefined.

length_utf8(String) -> length(unicode:characters_to_list(String, utf8)).

binarize_proplist(Pl) when is_list(Pl) ->
  lists:map(fun
              ({K,V}) -> {ensure_binary(K), ensure_binary(V)};
              (Any)   -> Any
            end, Pl);
binarize_proplist(Any) -> Any.

ensure_timestamp(none)                                 -> 0;
ensure_timestamp(undefined)                            -> 0;
ensure_timestamp(Timestamp) when is_integer(Timestamp) -> Timestamp.

ensure_list(undefined) -> undefined;
ensure_list(X) when is_list(X) -> X;
ensure_list(X) when is_binary(X) -> binary_to_list(X);
ensure_list(X) when is_atom(X) -> atom_to_list(X);
ensure_list(_) -> undefined.

ensure_binary(undefined)           -> undefined;
ensure_binary(X) when is_binary(X) -> X;
ensure_binary(X) when is_list(X)   -> list_to_binary(X);
ensure_binary(X) when is_integer(X)-> integer_to_binary(X);
ensure_binary(X) when is_atom(X)   -> atom_to_binary(X, utf8);
ensure_binary(_)                   -> <<>>.

ensure_float(undefined) -> undefined;
ensure_float(F)         -> float(F).

ensure_integer(undefined) -> undefined;
ensure_integer(F)         -> round(F).

ensure_boolean(undefined)                   -> false;
ensure_boolean(<<"true">>)                  -> true;
ensure_boolean(<<"false">>)                 -> false;
ensure_boolean("true")                      -> true;
ensure_boolean("false")                     -> false;
ensure_boolean(true)                        -> true;
ensure_boolean(false)                       -> false;
ensure_boolean(X) when is_integer(X), X > 0 -> true;
ensure_boolean(_)                           -> false.

binary_to_num(Bin) ->
  N = binary_to_list(Bin),
  case string:to_float(N) of
      {error,no_float} -> list_to_integer(N);
      {F,_Rest} -> F
  end.

get_binary_length(undefined)                     -> 0;
get_binary_length(Binary) when is_binary(Binary) ->
  length(unicode:characters_to_list(Binary, utf8)).

crop_str(undefined, _) ->  undefined;
crop_str(Binary, Size) ->
  List = unicode:characters_to_list(Binary, utf8),
  case length(List) > Size of
    true  ->
      {L1, _} = lists:split(Size, List),
      unicode:characters_to_binary(L1);
    false ->
      Binary
  end.

is_valid_phone(Input) ->
  Parsed = get_national_number(Input),
  length(integer_to_list(Parsed)) >= ?MINIMUM_NATIONAL_NUMBER_LENGTH.

format_phone(PhoneNumber) ->
  Parsed = get_national_number(PhoneNumber),
  Str = integer_to_list(Parsed),
  case length(Str) >= ?MINIMUM_NATIONAL_NUMBER_LENGTH of
    true  -> integer_to_binary(Parsed);
    false -> <<>>
  end.

format_lookup_value(undefined)                    -> undefined;
format_lookup_value(Value) when is_integer(Value) -> list_to_binary(integer_to_list(Value));
format_lookup_value(Value) when is_list(Value)    -> list_to_binary(Value);
format_lookup_value(Value) when is_binary(Value)  -> Value.

format_phone_with_code(undefined, Phone) ->
  FormatedPhone = format_phone(Phone),
  case get_county_code(Phone) of
    {ok, CountryCode} -> {ok, concat_phone(CountryCode, FormatedPhone)};
    {error, invalid}  -> {ok, <<?DEFAULT_COUNTRY_CODE/binary, FormatedPhone/binary>>}
  end;
format_phone_with_code(UserId, Phone) ->
  StrippedPhone = re:replace(format_utf8(Phone), "[^0-9]", "", [global, {return, list}]),
  Parsed = phonenumber_util:parse(ensure_plus(ensure_binary(StrippedPhone)), <<>>),

  case phonenumber_util:is_valid_number(Parsed) of
    true  ->
      {ok, phonenumber_util:format(Parsed, e164)};
    false ->
      UserId1 = parse_id(UserId),
      case im_roster_chat:get(UserId1) of
        undefined -> {error, not_valid};
        User ->
          ParsedUserPhone = phonenumber_util:parse(ensure_binary(User#im_usr.phone), <<>>),
          RegionCode = phonenumber_util:get_region_code_for_number(ParsedUserPhone),
          CountryCodeMasks = get_country_code_mask(RegionCode),
          case get_phone_by_mask(StrippedPhone, CountryCodeMasks) of
            {ok, FormatedPhone}  ->
              {ok, phonenumber_util:format(phonenumber_util:parse(FormatedPhone, RegionCode), e164)};
            _                    -> {error, not_valid}
          end
      end
  end.

get_national_number(Input) when is_integer(Input) -> get_national_number(integer_to_list(Input));
get_national_number(Input) when is_list(Input)    -> get_national_number(list_to_binary(Input));
get_national_number(Input) when is_binary(Input)  ->
  Parsed = phonenumber_util:parse(ensure_plus(Input), <<>>),
  National = case phonenumber:has_country_code(Parsed) of
    true  ->
      CountryCode = phonenumber:get_country_code(Parsed),
      RegionCode = phonenumber_util:get_region_code_for_country_code(CountryCode),
      phonenumber_util:parse(Input, RegionCode);
    false -> phonenumber_util:parse(Input, ?DEFAULT_REGION_CODE)
  end,
  phonenumber:get_national_number(National).

get_county_code(Input) when is_integer(Input) -> get_county_code(integer_to_list(Input));
get_county_code(Input) when is_list(Input)    -> get_county_code(list_to_binary(Input));
get_county_code(Input) ->
  Parsed = phonenumber_util:parse(ensure_plus(Input), <<>>),
  case phonenumber:has_country_code(Parsed) of
    true  ->
      CountryCode = phonenumber:get_country_code(Parsed),
      {ok, CountryCode};
    false ->
      {error, invalid}
  end.

strip(X) ->
  [ E || E <- X, not lists:member(E, "() !@#$%*_-+='`[{]}^\~?<>.,;:\t\n\'\"")].

trim(List) ->
  re:replace(ensure_list(List), "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

ensure_plus(<<"+", _/binary>>=Str) when is_binary(Str) -> Str;
ensure_plus(Str)                   when is_binary(Str) -> <<"+", Str/binary>>.

concat_phone(Code, Phone) ->
  BinaryCode = ensure_binary(Code),
  BinaryPhone = ensure_binary(Phone),
  <<"+", BinaryCode/binary, BinaryPhone/binary>>.

get_country_code_mask(RegionCode) ->
  CountryCode = phonenumber_util:get_country_code_for_region(RegionCode),
  CountryCodeList = lists:reverse(binary_to_list(ensure_binary(CountryCode))),
  [ lists:reverse(lists:sublist(CountryCodeList, E)) || E <- lists:seq(1, length(CountryCodeList)) ].

get_phone_by_mask(_Phone, []) -> {error, not_matched};
get_phone_by_mask(Phone, [Mask|T]) ->
  MLength = length(Mask),
  try Mask = lists:sublist(Phone, 1, MLength) of
    _ -> {ok, lists:sublist(Phone, MLength, length(Phone))}
  catch
    error:_ -> get_phone_by_mask(Phone, T)
  end.

proplist_to_tuple(undefined)           -> undefined;
proplist_to_tuple(Pl) when is_list(Pl) ->
  Len = length(Pl),
  proplist_to_tuple(Len, Pl, erlang:make_tuple(Len, undefined)).

proplist_to_tuple(0, _, Acc) -> Acc;
proplist_to_tuple(Index, Data, Acc) ->
  Val = proplists:get_value(erlang:integer_to_binary(Index), Data, <<>>),
  proplist_to_tuple(
    Index-1,
    Data,
    erlang:setelement(Index, Acc, Val)
  ).

tuple_to_proplist(undefined)                  -> undefined;
tuple_to_proplist(Tuple) when is_tuple(Tuple) ->
  Data = tuple_to_list(Tuple),
  BinSeq = lists:map(fun(E) ->
                       erlang:integer_to_binary(E)
                     end, lists:seq(1,length(Data))),
  lists:zip(BinSeq, Data).

json_to_mongo(undefined) -> undefined;
json_to_mongo([]) -> undefined;
json_to_mongo(String) ->
  {ok, Tuple} = yaws_json2:decode_string(ensure_list(String)),
  json_to_mongo_inner(Tuple).
json_to_mongo_inner({struct, Props}) ->
  list_to_tuple(lists:foldl(fun({Field, Value}, Acc) ->
    Acc ++ [format_utf8(Field), json_to_mongo_inner(Value)]
  end, [], Props));
json_to_mongo_inner({array, Items}) ->
  [json_to_mongo_inner(Item) || Item <- Items];
json_to_mongo_inner(Item) when is_list(Item) -> format_utf8(Item);
json_to_mongo_inner(Item) -> Item.

mongo_to_json(undefined) -> undefined;
mongo_to_json(Data) ->
  Transformed = case is_proplist(Data) of
    true -> mongo_to_json_inner(proplist_to_mongo_tuple(Data, []));
    false -> mongo_to_json_inner(Data)
  end,
  yaws_json2:encode(Transformed).

mongo_to_json_inner(Data) when is_tuple(Data)  -> mongo_to_json_struct(tuple_to_list(Data), []);
mongo_to_json_inner(Data) when is_list(Data)   -> [mongo_to_json_inner(Item) || Item <- Data];
mongo_to_json_inner(Data) when is_binary(Data) -> ensure_list(Data);
mongo_to_json_inner(Data)                      -> Data.

proplist_to_mongo_tuple([],               Acc) -> list_to_tuple(Acc);
proplist_to_mongo_tuple([{Key, Value}|T], Acc) -> proplist_to_mongo_tuple(T,  [Key | [Value | Acc]]).

mongo_tuple_to_proplist(Tuple) -> mongo_tuple_to_proplist(tuple_to_list(Tuple), []).
mongo_tuple_to_proplist([], Acc) -> Acc;
mongo_tuple_to_proplist([A,B|Rest], Acc) -> mongo_tuple_to_proplist(Rest, Acc ++ [{A,B}]).

mongo_to_json_struct([],              Acc) -> {struct, Acc};
mongo_to_json_struct([Field,Value|T], Acc) ->
  mongo_to_json_struct(T, Acc ++ [{ensure_list(Field), mongo_to_json_inner(Value)}]).

read_json_file(FileName) ->
  Json = ensure_list(read_lines(FileName)),
  case Json =/= "" of
    true ->
      case yaws_json2:decode_string(Json) of
        {ok, Decoded} -> Decoded;
        _             -> undefined
      end;
    false -> undefined
  end.

read_lines(FileName) ->
  case file:open(FileName, [read]) of
    {ok, Device} ->
      try get_all_lines(Device)
        after file:close(Device)
      end;
    _ -> ""
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

crypto_random_string() ->
  crypto_random_string(1.0e6, 1.0e9).
crypto_random_string(Count, Probability) ->
  Bits = entropy_string:bits(Count, Probability),
  entropy_string:random_string(Bits).


list_unique([])    -> [];
list_unique([H|T]) -> [H | [X || X <- T, X /= H]].

random_number(Min, Max) -> Min + round(rand:uniform() * (Max - Min)).

random_string(Length, chars)             -> generate(Length, ?CHARS);
random_string(Length, numbers)           -> generate(Length, ?NUMBERS);
random_string(Length, chars_and_numbers) -> generate(Length, lists:concat([?CHARS, ?NUMBERS])).

generate(Length, AllowedSymbols) ->
  MaxLength = length(AllowedSymbols),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(random_number(1, MaxLength), AllowedSymbols)] ++ Acc end,
    [], lists:seq(1, Length)
  ).

is_proplist(List) ->
  is_list(List) andalso
    lists:all(fun({_, _}) -> true;
      (_)      -> false
    end,
    List).

timestamp_to_datetime(Milliseconds) ->
  BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds       = BaseDate + (Milliseconds div 1000),
  calendar:gregorian_seconds_to_datetime(Seconds).

datetime_to_timestamp(DayTime) ->
  (calendar:datetime_to_gregorian_seconds(DayTime) - 62167219200) * 1000.

format_price(Price, Currency) when is_integer(Price) ->
  format_price(format_utf8(Price), Currency);
format_price(Price, Currency) ->
  case string:uppercase(ensure_list(Currency)) of
    "EUR" -> format_utf8( ensure_list(Price) ++ " " ++ "€");
    "USD" -> format_utf8( ensure_list(Price) ++ " " ++ "$");
    Currency1 -> format_utf8( ensure_list(Price) ++ " " ++ Currency1)
  end.

parse_boolean(true) -> true;
parse_boolean(1) -> true;
parse_boolean(<<"true">>) -> true;
parse_boolean("true") -> true;
parse_boolean(_) -> false.

base_url() ->
  Protocol = case im:is_debug() of
    true -> "http";
    false -> "https"
  end,
  {ok, Url}  = application:get_env(im, url),
  Protocol ++ "://" ++ im_common:ensure_list(Url).

proplist_upsert(Key, Value, List) ->
  case proplists:lookup(Key, List) of
    none -> List ++ [{Key, Value}];
    _    -> lists:keyreplace(Key, 1, List, {Key, Value})
  end.
