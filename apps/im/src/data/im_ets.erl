-module(im_ets).

-export([spec/0, handle/3]).
-export([all/1, exists/2, put/2, modify/3, get_one/2, delete/2]).

spec() -> im_std_worker:spec(ets, [], fun ?MODULE:handle/3).

all(EtsName)             -> im_std_worker:call(ets, {all, EtsName}).
get_one(EtsName, Id)     -> im_std_worker:call(ets, {get_one, EtsName, Id}).
put(EtsName, Tuple)      -> im_std_worker:call(ets, {put, EtsName, Tuple}).
modify(EtsName, Id, Fun) -> im_std_worker:call(ets, {modify, EtsName, Id, Fun}).
exists(EtsName, Id)      -> im_std_worker:call(ets, {exists, EtsName, Id}).
delete(EtsName, Id)      -> im_std_worker:call(ets, {delete, EtsName, Id}).

handle(call, {exists, EtsName, Id}, State) ->
  ensure_ets(EtsName),
  Result = try ets:lookup_element(EtsName, Id, 1) of
    Id -> true;
    _  -> false
  catch
    error:badarg -> false;
    _            -> false %% write to log
  end,
  {reply, Result, State};

handle(call, {all, EtsName}, State) ->
  ensure_ets(EtsName),
  {reply, ets:tab2list(EtsName), State};

handle(call, {get_one, EtsName, Id}, State) ->
  Result = get_one_inner(EtsName, Id),
  {reply, Result, State};

handle(call, {put, EtsName, Tuple}, State) ->
  case put_inner(EtsName, Tuple) of
    {ok, _} -> {reply, ok, State};
    {error, Reason} -> {reply, {error, Reason}}
  end;

handle(call, {modify, EtsName, Id, Fun}, State) ->
  case is_function(Fun) of
    true ->
      Record = get_one_inner(EtsName, Id),
      Tuple = Fun(Record),
      case is_tuple(Tuple) of
        true ->
          case put_inner(EtsName, Tuple) of
            {ok, _} ->
              {reply, Tuple, State};
            {error, Reason} ->
              {reply, {error, Reason}}
          end;
        false ->
          {reply, ok, State}
      end;
    false ->
      {reply, ok, State}
  end;

handle(call, {delete, EtsName, Id}, State) ->
  ensure_ets(EtsName),
  ets:delete(EtsName, Id),
  {reply, ok, State};

handle(_, _, _) -> ok.

ensure_ets(EtsName) ->
  case ets:info(EtsName) =:= undefined of
    true -> ets:new(EtsName, [set, named_table, public]);
    false -> skip
  end.

get_one_inner(EtsName, Id) ->
  ensure_ets(EtsName),
  case ets:lookup(EtsName, Id) of
    [H|_] ->
      case element(1, H) of
        Id -> H;
        _  -> undefined
      end;
    []    -> undefined
  end.

put_inner(EtsName, Tuple) ->
  case is_tuple(Tuple) of
    true ->
      ensure_ets(EtsName),
      ets:insert(EtsName, Tuple),
      {ok, Tuple};
    false ->
      {error, badarg}
  end.
