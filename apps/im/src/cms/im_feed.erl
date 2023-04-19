-module(im_feed).

-include("im_common.hrl").

-export([create_id_map/2, add_id/2, find_id/2, remove_id/2, resolve_id/4]).

create_id_map(TargetUserIds, UserId) ->
  PrimaryRecordId = ctail:next_id(),
  IdMap = lists:map(fun(TargetUserId) ->
    RecordIdForUser = case TargetUserId of
      UserId -> PrimaryRecordId;
      _      -> ctail:next_id()
    end,
    [TargetUserId, RecordIdForUser]
  end, TargetUserIds),
  {PrimaryRecordId, IdMap}.

add_id(TargetUserId, IdMap) ->
  NewRecordId = ctail:next_id(),
  NewIdMap = [[TargetUserId, NewRecordId]|IdMap],
  {NewRecordId, NewIdMap}.

find_id(TargetUserId, IdMap) ->
  case lists:filter(fun([UserId, _TaskId]) -> UserId =:= TargetUserId end, IdMap) of
    [[_TargetUserId, TaskId]] -> TaskId;
    _                  -> undefined
  end.

remove_id(TargetUserId, IdMap) ->
  lists:filter(fun([UserId, _TaskId]) -> UserId =/= TargetUserId end, IdMap).

resolve_id(_, undefined, _, _)                                        -> undefined;
resolve_id(Table, RecordId, IdMapElement, #im_usr{id=UserId})    -> resolve_id(Table, RecordId, IdMapElement, UserId);
resolve_id(Table, RecordId, IdMapElement, UserId) when is_list(Table) -> resolve_id(list_to_atom(Table), RecordId, IdMapElement, UserId);
resolve_id(Table, RecordId, IdMapElement, UserId) ->
  case ctail:get(Table, RecordId) of
    {ok, Record} ->
      Filtered = lists:filter(fun([TargetUserId, _]) ->
        UserId =:= TargetUserId
      end, element(IdMapElement, Record)),
      case Filtered of
        [] -> undefined;
        [[_, RecordIdForUser]] -> RecordIdForUser
      end;
    _ ->
      undefined
  end.
