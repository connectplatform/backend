-module(im_event).
-behaviour(gen_event).

-include("im_common.hrl").

%% gen_event callbacks
-export([code_change/3, handle_call/2, handle_event/2, handle_info/2, init/1, terminate/2]).
-export([fire/2, initialize/0]).

initialize() ->
  gen_event:start({local, ?EVENT_MANAGER}),
  gen_event:add_handler(?EVENT_MANAGER, ?MODULE, []).

fire(Event, Data) ->
  gen_event:notify(?EVENT_MANAGER, {Event, Data}).

init(_Args) ->
  {ok, {}}.

handle_event({?USER_UPDATED_EVENT, User = #im_usr{isVendor = IsVendor}}, State) ->
  case IsVendor =:= true of
    true -> skip;
    false -> im_elastic_search:upsert_user(User)
  end,

  case User#im_usr.data =/= undefined of
    true ->
      ProfileType = proplists:get_value(<<"profileType">>, im_common:mongo_tuple_to_proplist(User#im_usr.data)),

      case im_app_conf_watcher:get("segments") of
        {ok, {struct, List}} when is_list(List) ->
          Matched = lists:filter(fun({Key, _Value}) ->
            im_common:format_utf8(Key) =:= ProfileType
          end, List),

          case Matched of
            [{_, {struct, List2}}|_] ->
              case proplists:get_value("contacts", List2) of
                {array, Rules} when is_list(Rules) ->
                  FindResults = lists:map(fun({struct, Rule}) ->
                    Conditions=case proplists:get_value("conditions", Rule) of
                      {array, Conditions1} when is_list(Conditions1) ->
                        lists:map(fun({struct, [{"field", Field}, {"values", {array, Values}}]}) ->
                          {im_common:ensure_binary(Field), {<<"$in">>, lists:map(fun({struct, V}) ->
                            im_common:ensure_binary(proplists:get_value("value", V))
                          end, Values)}}
                        end, Conditions1);
                      _ -> skip
                    end,
                    Operator = case proplists:get_value("operator", Rule) of
                      "and" -> "$and";
                      "or"  -> "$or";
                      Op    -> Op
                    end,
                    Selector={im_common:ensure_binary(Operator), Conditions},
                    ctail_mongo:find(im_usr, Selector, 0)
                  end, Rules),

                  %% Merge results
                  UsersToAdd = lists:foldl(fun(FindResult, Acc) ->
                    lists:foldl(fun(U=#im_usr{id=UserId}, Acc1) ->
                      case lists:filter(fun(#im_usr{id=AccUserId}) -> AccUserId =:= UserId end, Acc1) of
                        [] -> [U|Acc1];
                        _  -> Acc1
                      end
                    end, Acc, FindResult)
                  end, [], FindResults),

                  %% Add contacts
                  lists:foreach(fun(#im_usr{id=UId}) ->
                    {ok, P1} = im_contact_fsm:start_link([User#im_usr.id, UId]),
                    {ok, P2} = im_contact_fsm:start_link([UId, User#im_usr.id]),
                    im_contact_fsm:add(P1, P2)
                  end, UsersToAdd);
                _ ->
                  skip
              end,

              case proplists:get_value("circles", List2) of
                {array, Circles} ->
                  lists:foreach(fun({struct, Props}) ->
                    Supervisor = proplists:get_value("supervisor", Props),
                    Params = proplists:get_value("params", Props),
                    im_circle:get(#'GetCircle'{supervisor=Supervisor, params=Params}, User#im_usr.id)
                  end, Circles);
                undefined -> skip
              end;
            [] -> skip
          end;
        _ -> skip
      end;
    false -> skip
  end,

  {ok, State};

handle_event(Event, State) ->
  io:format("[~p] unknown event ~p", [?MODULE, Event]),
  {ok, State}.

handle_call(Request, State) ->
  {ok, Request, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Args, _State) -> ok.
