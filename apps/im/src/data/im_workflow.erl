-module(im_workflow).

-include("im_common.hrl").

-export([get_state/2, start/2, transit/2, get/2, create/2, ensure_sys_workflows/0]).

%% Records

-record(definition, {
  version,
  target,
  attributes,
  setAttributes,
  startSteps,
  steps
}).

-record(step, {
  name,
  allowedSteps,
  setAttributesBefore,
  setAttributesAfter,
  requiredAttributes
}).

-record(attribute, {
  name,
  value
}).

-record(state, {
  attributes,
  data=[]
}).

%% API

get_state(#'GetWorkflowState'{ref=Ref, name=Name, entityId=EntityId}, UserId) ->
  case find_state(Name, EntityId) of
    {ok, State} ->
      case im_acl:has_super_perms(UserId) of
        true -> #'GetWorkflowStateResp'{ref=Ref, state=format_state(State)};
        false ->
          IsGranted = State#im_workflow_state.target =:= <<"user">> andalso UserId =:= im_common:parse_id(EntityId),
          case IsGranted of
            true -> #'GetWorkflowStateResp'{ref=Ref, state=format_state(State)};
            false -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
          end
      end;
    {error, _} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND, message="Workflow state not found"}
  end.

apply_attributes(SetAttributes, Attributes) ->
  lists:foldl(fun(#attribute{name=Key, value=Value}, Attrs) ->
    im_common:proplist_upsert(Key, Value, Attrs)
  end, Attributes, SetAttributes).

validate_attributes(Attributes, StepDef) ->
  MissingAttrs = lists:filter(fun(Attr) ->
    proplists:get_value(Attr, Attributes) =:= undefined
  end, StepDef#step.requiredAttributes),
  MissingAttrs =:= [].

start(#'StartWorkflow'{ref=Ref, name=Name, entityId=EntityId, step=Step, setAttributes=SetAttributes}, UserId) ->
  case im_acl:has_super_perms(UserId) of
    true ->
      case ctail:get(im_workflow, im_common:format_utf8(Name)) of
        {ok, Workflow} ->
          Now = sm:now(),
          Def = parse_definition(Workflow, Workflow#im_workflow.version),

          {ok, StepDef} = find_definition_step(Def, Step),

          SetAttributes1 = [parse_definition_attribute(Attr) || Attr <- SetAttributes],

          Attributes = [{AttrName, undefined} || AttrName <- Def#definition.attributes],
          Attributes1 = apply_attributes(Def#definition.setAttributes, Attributes),
          Attributes2 = apply_attributes(StepDef#step.setAttributesBefore, Attributes1),
          Attributes3 = apply_attributes(SetAttributes1, Attributes2),

          StateData = #state{attributes=[#attribute{name=Key, value=Value} || {Key, Value} <- Attributes3]},

          case validate_attributes(Attributes3, StepDef) of
            true ->
              case lists:member(im_common:format_utf8(Step), Def#definition.startSteps) of
                true ->
                  NewState = #im_workflow_state{id=state_id(Name, EntityId),
                    name=im_common:format_utf8(Name),
                    entityId=im_common:format_id(EntityId),
                    target=Def#definition.target,
                    version=Def#definition.version,
                    step=im_common:format_utf8(Step),
                    json=format_state_data(StateData),
                    created=Now,
                    updated=Now,
                    active=true},
                  ctail:put(NewState),

                  im_bot_update:send_system_message({workflow_state_changed, NewState}, UserId),

                  #'StartWorkflowResp'{ref=Ref, state=format_state(NewState)};
                false ->
                  #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="Specified step is not in 'startSteps'"}
              end;
            false ->
              #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="Missing required attributes"}
          end;
        {error, _} ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND, message="Workflow not found"}
      end;
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

transit(#'TransitWorkflow'{ref=Ref, name=Name, entityId=EntityId, step=Step, setAttributes=SetAttributes}, UserId) ->
  Name1 = im_common:format_utf8(Name),
  Step1 = im_common:format_utf8(Step),

  case im_acl:has_super_perms(UserId) of
    true ->
      case ctail:get(im_workflow, Name1) of
        {ok, Workflow} ->
          case ctail:get(im_workflow_state, state_id(Name, EntityId)) of
            {ok, State} ->
              Now = sm:now(),
              Def = parse_definition(Workflow, State#im_workflow_state.version),

              {ok, CurStepDef} = find_definition_step(Def, State#im_workflow_state.step),
              {ok, NewStepDef} = find_definition_step(Def, Step1),

              case State#im_workflow_state.active =:= true of
                true ->
                  case lists:member(Step1, CurStepDef#step.allowedSteps) of
                    true ->
                      StateData = parse_state_data(State#im_workflow_state.json),

                      SetAttributes1 = [parse_definition_attribute(Attr) || Attr <- SetAttributes],

                      Attributes = [{Key, Value} || #attribute{name=Key, value=Value} <- StateData#state.attributes],
                      Attributes1 = apply_attributes(CurStepDef#step.setAttributesAfter, Attributes),
                      Attributes2 = apply_attributes(NewStepDef#step.setAttributesBefore, Attributes1),
                      Attributes3 = apply_attributes(SetAttributes1, Attributes2),

                      NewStateData = StateData#state{attributes=[#attribute{name=Key, value=Value} || {Key, Value} <- Attributes3]},

                      IsActive = NewStepDef#step.allowedSteps =/= [<<"ended">>],

                      NewState = State#im_workflow_state{step=Step1,
                        json=format_state_data(NewStateData),
                        updated=Now,
                        active=IsActive},
                      ctail:put(NewState),

                      im_bot_update:send_system_message({workflow_state_changed, NewState}, UserId),

                      #'TransitWorkflowResp'{ref=Ref, state=format_state(NewState)};
                    false ->
                      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="Transition to this step is not allowed"}
                  end;
                false ->
                  #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="Workflow is ended"}
              end;
            {error, _} ->
              #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND, message="Workflow state not found"}
          end;
        {error, _} ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND, message="Workflow not found"}
      end;
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

get(#'GetWorkflow'{ref=Ref, name=Name}, UserId) ->
  case im_acl:has_super_perms(UserId) of
    true ->
      case ctail:get(im_workflow, im_common:format_utf8(Name)) of
        {ok, Workflow} ->
          #'GetWorkflowsResp'{ref=Ref, workflow=format(Workflow)};
        {error, _} ->
          #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND, message="Workflow not found"}
      end;
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

create(#'CreateWorkflow'{ref=Ref, name=Name, json=Json}, UserId) ->
  case save(Name, Json, im_acl:has_super_perms(UserId), false) of
    {ok, Workflow} ->
      #'CreateWorkflowResp'{ref=Ref, workflow=format(Workflow)};
    {error, version_exists} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="Workflow version already exists"};
    {error, checks_failed} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE, message="Workflow is invalid. Checks didn't pass"};
    {error, permission_denied} ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

%% Internal

version_id(Name, Version) ->
  {im_common:format_utf8(Name), Version}.

state_id(Name, EntityId) ->
  {im_common:format_utf8(Name), im_common:format_id(EntityId)}.

parse_definition(#im_workflow{name=Name}, StateVersion) ->
  {ok, FoundVersion} = ctail:get(im_workflow_version, version_id(Name, StateVersion)),
  {ok, {struct, Props}} = yaws_json2:decode_string(im_common:ensure_list(FoundVersion#im_workflow_version.json)),

  #definition{version=proplists:get_value("version", Props),
    target=im_common:format_utf8(proplists:get_value("target", Props)),
    attributes=im_common:format_utf8_array(json_array_or_empty("attributes", Props)),
    startSteps=im_common:format_utf8_array(json_array_or_empty("startSteps", Props)),
    steps=[parse_definition_step(Step) || Step <- json_array_or_empty("steps", Props)],
    setAttributes=[parse_definition_attribute(Attr) || Attr <- json_array_or_empty("setAttributes", Props)]}.

parse_definition_step({struct, Props}) ->
  #step{name=im_common:format_utf8(proplists:get_value("name", Props)),
    allowedSteps=im_common:format_utf8_array(json_array_or_empty("allowedSteps", Props)),
    setAttributesBefore=[parse_definition_attribute(Attr) || Attr <- json_array_or_empty("setAttributesBefore", Props)],
    setAttributesAfter=[parse_definition_attribute(Attr) || Attr <- json_array_or_empty("setAttributesAfter", Props)],
    requiredAttributes=im_common:format_utf8_array(json_array_or_empty("requiredAttributes", Props))}.

find_definition_step(Def, Step) ->
  StepName = im_common:format_utf8(Step),
  Steps = lists:filter(fun(Step1) -> Step1#step.name =:= StepName end, Def#definition.steps),
  case Steps of
    [] -> {error, not_found};
    [FoundStep|_] -> {ok, FoundStep}
  end.

parse_definition_attribute(#'WorkflowAttrEntity'{name=Name, value=Value}) ->
  parse_definition_attribute({Name, Value});
parse_definition_attribute({struct, Props}) ->
  parse_definition_attribute({proplists:get_value("name", Props), proplists:get_value("value", Props)});
parse_definition_attribute({Name, Value}) ->
  #attribute{name=im_common:format_utf8(Name),
    value=Value}.

parse_state_data(Json) ->
  {ok, {struct, Props}} = yaws_json2:decode_string(im_common:ensure_list(Json)),
  #state{
    attributes=[parse_definition_attribute(Attr) || Attr <- json_array_or_empty("attributes", Props)],
    data=lists:keydelete("attributes", 1, Props)}.

format_state_data(#state{attributes=Attributes, data=Data}) ->
  Attributes1 = [{struct, [{<<"name">>, Name}, {<<"value">>, Value}]} || #attribute{name=Name, value=Value} <- Attributes],
  im_common:format_utf8(yaws_json2:encode({struct, Data ++ [{<<"attributes">>, {array, Attributes1}}]})).

find_state(Name, EntityId) ->
  ctail:get(im_workflow_state, state_id(Name, EntityId)).

format(#im_workflow{id=Name, target=Target, version=Version, json=Json}) ->
  JsonString = lists:flatten(im_common:ensure_list(im_common:mongo_to_json(Json))),
  {ok, {struct, Props}} = yaws_json2:decode_string(JsonString),

  #'WorkflowEntity'{name=im_common:format_utf8(Name),
    target=im_common:format_utf8(Target),
    version=Version,
    attributes=im_common:format_utf8_array(json_array_or_empty("attributes", Props)),
    startSteps=im_common:format_utf8_array(json_array_or_empty("startSteps", Props)),
    steps=[format_step(Step) || Step <- json_array_or_empty("steps", Props)],
    json=im_common:format_utf8(JsonString)}.

format_state(#im_workflow_state{step=Step, name=Name, version=Version, json=Json, active=Active}) ->
  Data = parse_state_data(Json),
  #'WorkflowStateEntity'{
    name=im_common:format_utf8(Name),
    version=Version,
    attributes=[format_attr(Attr) || Attr <- Data#state.attributes],
    step=im_common:format_utf8(Step),
    json=Json,
    active=im_common:ensure_boolean(Active)}.

format_step({struct, Props}) ->
  #'WorkflowStepEntity'{name=im_common:format_utf8(proplists:get_value("name", Props)),
    allowedSteps=im_common:format_utf8_array(json_array_or_empty("allowedSteps", Props)),
    setAttributesBefore=[format_attr(Attr) || Attr <- json_array_or_empty("setAttributesBefore", Props)],
    setAttributesAfter=[format_attr(Attr) || Attr <- json_array_or_empty("setAttributesAfter", Props)],
    requiredAttributes=im_common:format_utf8_array(json_array_or_empty("requiredAttributes", Props))}.

format_attr({struct, Props}) ->
  Attr = #attribute{name=im_common:format_utf8(proplists:get_value("name", Props)),
    value=format_attr_value(proplists:get_value("value", Props))},
  format_attr(Attr);
format_attr(#attribute{name=Name, value=Value}) ->
  #'WorkflowAttrEntity'{name=im_common:format_utf8(Name), value=format_attr_value(Value)}.

format_attr_value(null) -> undefined;
format_attr_value(Value) -> Value.

json_array_or_empty(Field, Props) ->
  case proplists:get_value(Field, Props) of
    {array, List} -> List;
    _ -> []
  end.

ensure_sys_workflows() ->
  im_logger:debug(undefined, "[Workflow] Loading sys workflows"),

  {ok, PrivDir} = application:get_env(im, priv_folder),
  {ok, Listing} = file:list_dir(PrivDir ++ "/workflows"),

  lists:foreach(fun(FileName) ->
    FullName = PrivDir ++ "/workflows/" ++ FileName,
    Json = im_common:ensure_list(im_common:read_lines(FullName)),
    case Json =:= "" of
      true ->
        im_logger:error(undefined, "Unable to read workflow file: ~p", [FullName]);
      false ->
        Name = string:substr(FileName, 1, length(FileName) - 5),
        save(Name, Json, true, true)
    end
  end, Listing).

save(Name, Json, HasSuperPerms, IsRunBySystem) ->
  IsGranted = HasSuperPerms orelse IsRunBySystem,
  case IsGranted of
    true ->
      {ok, {struct, Props}} = yaws_json2:decode_string(im_common:ensure_list(Json)),

      Version = proplists:get_value("version", Props),
      Target = im_common:format_utf8(proplists:get_value("target", Props)),
      Steps = case proplists:get_value("steps", Props) of
        {array, Steps1} -> Steps1;
        _ -> []
      end,
      StartSteps = case proplists:get_value("startSteps", Props) of
        {array, StartSteps1} -> StartSteps1;
        _ -> []
      end,

      ExistingWorkflow = case ctail:get(im_workflow, im_common:format_utf8(Name)) of
        {ok, Workflow} -> Workflow;
        {error, _} -> undefined
      end,

      CurrentVersion = case ExistingWorkflow of
        undefined -> 0;
        _ ->
          case im:is_debug() of
            true -> 0;
            false -> ExistingWorkflow#im_workflow.version
          end
      end,

      Checks = [
        is_integer(Version),
        Version > CurrentVersion,
        lists:member(Target, [<<"user">>]),
        is_list(Steps) andalso length(Steps) > 0,
        is_list(StartSteps) andalso length(StartSteps) > 0
      ],
      FailsCount = length(lists:filter(fun(Result) -> Result =:= false end, Checks)),

      case FailsCount =:= 0 of
        true ->
          Now = sm:now(),

          VersionId = version_id(Name, Version),
          VersionResult = case ctail:get(im_workflow_version, VersionId) of
            {ok, ExistingVersion} ->
              case IsRunBySystem of
                true ->
                  UpdatedVersion = ExistingVersion#im_workflow_version{json=im_common:format_utf8(Json), updated=Now},
                  ctail:put(UpdatedVersion);
                false ->
                  {error, version_exists}
              end;
            {error, _} ->
              NewVersion = NewVersion = #im_workflow_version{id=VersionId,
                json=im_common:format_utf8(Json),
                created=Now,
                updated=Now},
              ctail:put(NewVersion)
          end,

          case VersionResult of
            ok ->
              NewWorkflow = case ExistingWorkflow =:= undefined of
                true ->
                  #im_workflow{id=im_common:format_utf8(Name),
                    name=im_common:format_utf8(Name),
                    target=Target,
                    version=Version,
                    json=im_common:json_to_mongo(Json),
                    created=Now,
                    updated=Now,
                    isSystem=IsRunBySystem =:= true};
                false ->
                  ExistingWorkflow#im_workflow{target=Target,
                    version=Version,
                    json=im_common:json_to_mongo(Json),
                    updated=Now}
              end,
              ctail:put(NewWorkflow),
              {ok, NewWorkflow};
            {error, Reason} ->
              {error, Reason}
          end;
        false ->
          im_logger:error(undefined, "[Workflow] Failed to load workflow ~p, invalid file. Checks: ~p, file: ~p", [Name, Checks, Json]),
          {error, checks_failed}
      end;
    false ->
      {error, permission_denied}
  end.
