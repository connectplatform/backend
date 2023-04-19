-module(im_department).

-include("im_common.hrl").

-export([list/1, c/1, r/1, u/2, d/1]).
-export([get/2]).

get(#'Department'{ref=Ref}, _UserId) ->
  #'DepartmentResp'{
    ref=Ref,
    departments = [ format_department(Department) || Department <- ctail:all(im_department)]
  }.

list({Skip, Limit, QueryString}) ->
  QueryString1 = im_common:ensure_binary(QueryString),
  Query = #{<<"name">>  => #{<<"$regex">> => QueryString1, <<"$options">> => <<"i">>}},
  Order = #{
    <<"$orderby">> => #{
      <<"name">> => 1
    }
  },

  Total = ctail_mongo:exec(count, [<<"im_department">>, Query]),

  Resp = [ format_department(D) || D <- ctail_mongo:find(im_department, maps:merge(#{<<"$query">> => Query}, Order), Skip, Limit)],

  {Total, Resp}.

c(Name) ->
  case ctail_mongo:find(im_department, #{<<"name">> => im_common:ensure_binary(Name)}, 0, 1) of
    [] -> ctail:put(#im_department{id=ctail:next_id(), name=Name});
    _  -> {err, <<"Error">>}
  end.

r(Id) ->
  case ctail:get(im_department, im_common:parse_id(Id)) of
    {ok, Department} -> format_department(Department);
    _               -> {err, <<"Error">>}
  end.

u(Id, Name) ->
  case ctail_mongo:find(im_department, #{<<"name">> => im_common:ensure_binary(Name)}, 0, 1) of
    [] ->
      ParsedId = im_common:parse_id(Id),
      {ok, Department} = ctail:get(im_department, ParsedId),
      ctail:put(Department#im_department{name=Name});
    _  -> {err, <<"Error">>}
  end.

d(Id) ->
  FormattedId = im_common:format_id(Id),
  case ctail_mongo:find(im_directory, #{<<"departmentId">> => FormattedId}, 0, 1) of
    [] -> ctail:delete(im_department, im_common:parse_id(FormattedId));
    _  -> {err, <<"Error">>}
  end.

format_department(#im_department{id=Id, name=Name}) ->
  #'DepartmentEntity'{
    id              = im_common:format_id(Id),
    name            = im_common:format_utf8(Name)
  }.
