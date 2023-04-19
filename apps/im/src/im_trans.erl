-module(im_trans).

-export([init/0]).
-export([t/1, t/2, t/3]).

-define(TRANS_KEY, trans_data).
-define(FALLBACK_LANG, <<"en">>).

init() ->
  case ets:info(?MODULE) of
    undefined ->
      FilePath = filename:join(
        filename:dirname(filename:dirname(code:which(?MODULE))),
        "priv/translations/translations.json"
      ),
      {ok, Bin} = file:read_file(FilePath),
      Json = binary_to_list(Bin),
      {ok, {struct, Data}} = yaws_json2:decode_string(Json),

      ets:new(?MODULE, [set, named_table, public]),
      ets:insert(?MODULE, {?TRANS_KEY, Data});
    _ -> skip
  end.

% https://view.officeapps.live.com/op/embed.aspx?src=https://calibre-ebook.com/downloads/demos/demo.docx
% https://docs.google.com/viewer?url=https://calibre-ebook.com/downloads/demos/demo.docx

t(Key)                           -> t(?FALLBACK_LANG, Key).
t(Key, Args) when is_list(Args)  -> t(?FALLBACK_LANG, Key, Args);
t(Lang, Key) when is_binary(Key) ->
  Trans = ts(Lang),
  case lists:filter(fun({K,_T}) -> K =:= Key end, Trans) of
    []          -> "";
    [{Key,T}|_] -> T
  end.

t(Lang, Key, [])   -> t(Lang, Key);
t(Lang, Key, Args) -> replace(t(Lang, Key), Args).

replace(String, Args)               -> replace(String, 1, length(Args), Args).
replace(String, C, N, _) when C > N -> String;
replace(String, C, N, Args)         ->
  %% android string placeholder %$1s or %$02d
  RegEx = list_to_binary("\\%" ++ integer_to_list(C) ++ "\\$.*?(s|d)"),
  Replacement = lists:nth(C, Args),
  Input1 = re:replace(String, RegEx, Replacement, [global,unicode,{return,binary}]),

  replace(Input1, C+1, N, Args).

ts(LangRaw) ->
  Lang = ensure_lang(LangRaw),
  {_L, Trans} = hd(lists:filter(fun({L,_}) -> L =:= Lang end, all())),
  Trans.

ensure_lang(Lang) ->
  case lists:member(Lang, langs()) of
    true  -> Lang;
    false -> ?FALLBACK_LANG
  end.

langs() -> [L || {L,_} <- all()].

all() ->
  [ {list_to_binary(L), [{list_to_binary(K), list_to_binary(V)} || {K,V} <- T]} || {L, {struct, T}} <- data()].

data() ->
  case ets:lookup(?MODULE, ?TRANS_KEY) of
    [{?TRANS_KEY,Data}|_] -> Data;
    []                    -> [] %% write to log
  end.
