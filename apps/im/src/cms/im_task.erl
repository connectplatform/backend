-module(im_task).

-include("im_common.hrl").

-export([get/2, create/2, update/2, assign/2, status/2, delete/2]).

get(#'Task'{ref=Ref, status=Status, skip=Skip, limit=Limit, feedType=FeedType, feedId=FeedId}, UserId) ->
  Limit1 = case Limit =:= undefined orelse Limit =:= 0 of true -> 30; _ -> Limit end,
  Skip1 = case Skip of undefined -> 0; _ -> Skip end,

  Selector = case Status =:= undefined of
    true -> {};
    false -> {<<"status">>, im_common:ensure_binary(Status)}
  end,

  FilterByChat = FeedType =/= undefined andalso FeedType =/= 0,
  Selector1 = case FilterByChat of
    true ->
      Granted = case FeedType of
        ?MESSAGE_FEED_TYPE_CHAT -> true;
        ?MESSAGE_FEED_TYPE_ROOM ->
          Room = im_roster_muc:get(im_common:parse_id(FeedId)),
          lists:member(UserId, Room#im_grp.members)
      end,
      case Granted of
        true ->
          MsgFeedId = im_chatupdate:msg_feed_to_roster(FeedType, FeedId, UserId),
          list_to_tuple(tuple_to_list(Selector) ++ [<<"feedId">>, tuple_to_list(MsgFeedId)]);
        false -> permisson_denied
      end;
    false -> list_to_tuple(tuple_to_list(Selector) ++ [<<"$or">>, [{<<"assignee">>, UserId}, {<<"reporter">>, UserId}]])
  end,

  case Selector1 of
    permisson_denied ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED};
    _ ->
      Query = {<<"$query">>, Selector1, <<"$orderby">>, {<<"created">>, -1}},

      Total = ctail_mongo:exec(count, [<<"im_task">>, Selector1]),
      Tasks = ctail_mongo:find(im_task, Query, Skip1, Limit1),
      TaskEntities = [im_dto:format_task(Task, UserId) || Task <- Tasks],

      #'TaskResp'{ref=Ref, tasks=TaskEntities, total=Total}
  end.

create(#'CreateTask'{ref=Ref, task=TaskEntity}, UserId) ->
  Task = im_dto:parse_task(TaskEntity),

  Status = case Task#im_task.status =/= undefined of
    true -> Task#im_task.status;
    false -> "todo"
  end,

  FilterByChat = TaskEntity#'TaskEntity'.feedType =/= undefined andalso TaskEntity#'TaskEntity'.feedId =/= undefined,
  FeedId = case FilterByChat of
    true ->
      FeedId1 = im_common:parse_id(TaskEntity#'TaskEntity'.feedId),
      Granted = case TaskEntity#'TaskEntity'.feedType of
        ?MESSAGE_FEED_TYPE_CHAT -> true;
        ?MESSAGE_FEED_TYPE_ROOM ->
          Room = im_roster_muc:get(FeedId1),
          lists:member(UserId, Room#im_grp.members)
      end,
      case Granted of
        true -> im_chatupdate:msg_feed_to_roster(TaskEntity#'TaskEntity'.feedType, FeedId1, UserId);
        false -> undefined
      end;
    false -> undefined
  end,

  NewFeedId = case FeedId of
    undefined -> undefined;
    _         -> tuple_to_list(FeedId)
  end,

  Task1 = Task#im_task{id=ctail:next_id(),
    reporter=UserId,
    status=Status,
    feedId=NewFeedId,
    created=sm:now()},

  ctail:put(Task1),

  TargetUserIds = case Task#im_task.assignee =:= UserId of
    true -> [UserId];
    false -> [UserId, Task#im_task.assignee]
  end,

  lists:foreach(fun(TargetUserId) ->
    im_message:create_update(TargetUserId, #im_update{type=?UPDATE_TYPE_TASK, taskId=Task1#im_task.id})
  end, TargetUserIds),

  case Task#im_task.assignee =/= UserId of
    true -> send_task_assigned_push(Task1);
    false -> skip
  end,

  #'CreateTaskResp'{ref=Ref, task=im_dto:format_task(Task1, UserId)}.

update(#'UpdateTask'{ref=Ref, id=TaskId, payload=Payload, media=Media}, UserId) ->
  {ok, Task} = ctail:get(im_task, im_common:parse_id(TaskId)),

  IsGranted = Task#im_task.reporter =:= UserId orelse Task#im_task.assignee =:= UserId,
  case IsGranted of
    true ->
      UpdatedTask = Task#im_task{
        payload=im_common:format_utf8(Payload),
        media=[im_dto:parse_media(M) || M <- Media]},
      ctail:put(UpdatedTask),

      TargetUserIds = case Task#im_task.assignee =:= UserId of
        true -> [UserId];
        false -> [UserId, Task#im_task.assignee]
      end,

      lists:foreach(fun(TargetUserId) ->
        im_message:create_update(TargetUserId, #im_update{type=?UPDATE_TYPE_TASK, taskId=Task#im_task.id})
      end, TargetUserIds),

      #'UpdateTaskResp'{ref=Ref};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

assign(#'AssignTask'{ref=Ref, id=TaskId, assignee=Assignee}, UserId) ->
  {ok, Task} = ctail:get(im_task, im_common:parse_id(TaskId)),

  NewAssignee = im_common:parse_id(Assignee),
  IsGranted = Task#im_task.reporter =:= UserId orelse Task#im_task.assignee =:= UserId,

  case IsGranted =:= true of
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED};
    true ->
      case Task#im_task.assignee =:= NewAssignee of
        true -> skip;
        false ->
          UpdatedTask = Task#im_task{
            assignee=NewAssignee,
            updated=sm:now()},
          ctail:put(UpdatedTask),

          NewAssigneeIsNotReporter = NewAssignee =/= Task#im_task.reporter,
          case NewAssigneeIsNotReporter of
            true ->
              send_task_assigned_push(Task),
              im_message:create_update(NewAssignee, #im_update{type=?UPDATE_TYPE_TASK, taskId=Task#im_task.id});
            false -> skip
          end,

          PrevAssigneeIsNotReporter = Task#im_task.assignee =/= Task#im_task.reporter,
          case PrevAssigneeIsNotReporter of
            true -> im_message:create_update(Task#im_task.assignee, #im_update{type=?UPDATE_TYPE_DELETE_TASKS, ids=[Task#im_task.id]});
            false -> skip
          end,

          im_message:create_update(UserId, #im_update{type=?UPDATE_TYPE_TASK, taskId=Task#im_task.id})
      end,
      #'AssignTaskResp'{ref=Ref}
  end.

status(#'UpdateTaskStatus'{ref=Ref, id=TaskId, status=Status}, UserId) ->
  {ok, Task} = ctail:get(im_task, im_common:parse_id(TaskId)),

  IsGranted = Task#im_task.reporter =:= UserId orelse Task#im_task.assignee =:= UserId,
  case IsGranted of
    true ->
      Status1 = im_common:format_utf8(Status),
      case Status1 =:= Task#im_task.status of
        true -> skip;
        false ->
          UpdatedTask = Task#im_task{status=Status1},
          ctail:put(UpdatedTask),

          im_message:create_update(Task#im_task.reporter, #im_update{type=?UPDATE_TYPE_TASK, taskId=Task#im_task.id}),

          case Task#im_task.assignee =/= Task#im_task.reporter of
            true -> im_message:create_update(Task#im_task.assignee, #im_update{type=?UPDATE_TYPE_TASK, taskId=Task#im_task.id});
            false -> skip
          end,

          case Status1 =:= <<"done">> andalso Task#im_task.assignee =/= Task#im_task.reporter of
            true ->
              % case Task#im_task.feedId =/= undefined andalso Task#im_task.reporter =/= Task#im_task.assignee of
              %   true ->
              %     {FeedType, FeedId} = im_chatupdate:roster_to_msg_feed(Task#im_task.feedId, Task#im_task.assignee),
              %     im_message:send_sys_msg(Task#im_task.assignee, FeedType, FeedId, <<"task.message.done">>, [im_message:markup_user(Task#im_task.assignee), Task#im_task.payload]);
              %   false ->
              %     im_message:send_from_sys_user(Task#im_task.reporter, <<"task.message.done">>, [im_message:markup_user(Task#im_task.assignee), Task#im_task.payload])
              % end,
              send_task_completed_push(Task);
            false -> skip
          end
      end,
      #'UpdateTaskStatusResp'{ref=Ref};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

delete(#'DeleteTask'{ref=Ref, id=TaskId}, UserId) ->
  {ok, Task} = ctail:get(im_task, im_common:parse_id(TaskId)),

  IsGranted = Task#im_task.reporter =:= UserId,

  case IsGranted of
    true ->
      ctail:delete(im_task, Task#im_task.id),

      im_message:create_update(Task#im_task.reporter, #im_update{type=?UPDATE_TYPE_DELETE_TASKS, ids=[Task#im_task.id]}),

      case Task#im_task.assignee =/= Task#im_task.reporter of
        true -> im_message:create_update(Task#im_task.assignee, #im_update{type=?UPDATE_TYPE_DELETE_TASKS, ids=[Task#im_task.id]});
        false -> skip
      end,

      #'DeleteTaskResp'{ref=Ref};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

send_task_assigned_push(Task=#im_task{payload=Payload}) ->
  ReporterIsAssignee = Task#im_task.assignee =:= Task#im_task.reporter,
  case ReporterIsAssignee of
    true -> skip;
    false ->
      IOSAlertText = im_trans:t(im_locale:get(Task#im_task.assignee), <<"push.message.task.assigned">>, [Payload]),
      AndroidJson = [{<<"taskId">>, im_common:format_id(Task#im_task.id)}],
      im_push_ios:send_alert(Task#im_task.assignee, IOSAlertText, <<"task">>),
      im_push_web:send_alert(Task#im_task.assignee, IOSAlertText, <<"task">>),
      im_push:send_json(Task#im_task.assignee, ?PLATFORM_ANDROID, [{<<"data">>, AndroidJson}, {<<"priority">>, <<"high">>}, {<<"collapse_key">>, <<"task">>}])
  end.

send_task_completed_push(Task=#im_task{payload=Payload}) ->
  case Task#im_task.assignee =/= Task#im_task.reporter of
    true ->
      IOSAlertText = im_trans:t(im_locale:get(Task#im_task.reporter), <<"push.message.task.completed">>, [Payload]),
      AndroidJson = [{<<"taskId">>, im_common:format_id(Task#im_task.id)}],
      im_push_ios:send_alert(Task#im_task.reporter, IOSAlertText, <<"task">>),
      im_push_web:send_alert(Task#im_task.reporter, IOSAlertText, <<"task">>),
      im_push:send_json(Task#im_task.reporter, ?PLATFORM_ANDROID, [{<<"data">>, AndroidJson}, {<<"priority">>, <<"high">>}, {<<"collapse_key">>, <<"task">>}]);
    false ->
      skip
  end.
