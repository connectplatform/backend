-module(im_elastic_search).

-include("im_common.hrl").

-define(ELASTIC_USER_IDX, <<"users">>).
-define(ELASTIC_LOG_IDX, <<"logs">>).
-define(USER_DOC, <<"user">>).
-define(MIN_SEARCH_WORLD_LENGTH, 3).
-define(MIN_SEARCH_PHONE_LENGTH, 4).

-export([find_user/2, ensure_index/0, upsert_user/1, reindex_users/0, drop_and_reindex_users/0]).

find_user(#'FindUser'{ref = Ref, name = Name, isVendor = IsVendor, userIds = []}, UserId) ->
  Fun = fun() ->
    case index_exists(?ELASTIC_USER_IDX) of
      false -> drop_and_reindex_users();
      true -> skip
    end,
    case find_users_by_query(Name, IsVendor, [], UserId) of
      {ok, Contacts} ->
        Contacts1 = lists:map(fun(Contact) ->
          case im_acl:has_role("super_admin", UserId) of
            true -> Contact;
            false ->
              Contact#'ContactEntity'{phone = undefined, originalPhone = undefined, skype = undefined, email = undefined}
          end
        end, Contacts),
        #'FindUserResp'{ref = Ref, contacts = Contacts1};
      {error, Code} ->
        #'ErrorResp'{ref = Ref, code = Code}
    end
  end,

  try Fun() catch
    Error:Reason ->
      im_logger:error(undefined, "[Elastica] find_user terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end;
find_user(#'FindUser'{ref = Ref, userIds = UserIds}, UserId) when is_list(UserIds), length(UserIds) > 0 ->
  im_logger:error(undefined, "[Elastica] find_user by userIds from Mongodb", []),
  Contacts = lists:foldl(fun(Id, Acc) ->
    case ctail:get(im_usr, im_common:parse_id(Id)) of
      {ok, User} -> [im_contact:format(User) | Acc];
      {error, _} -> Acc
    end
  end, [], UserIds),
  Contacts1 = lists:map(fun(Contact) ->
    case im_acl:has_role("super_admin", UserId) of
      true -> Contact;
      false ->
        Contact#'ContactEntity'{phone = undefined, originalPhone = undefined, skype = undefined, email = undefined}
    end
  end, Contacts),
  #'FindUserResp'{ref = Ref, contacts = Contacts1};
find_user(#'FindUser'{ref = Ref}, _) ->
  #'ErrorResp'{ref = Ref, code = ?ERROR_CODE_INVALID_MESSAGE}.

ensure_index() ->
  Fun = fun() ->
    AllowRWSetting = {<<"index">>, [
      {<<"blocks">>, [
        {<<"read_only_allow_delete">>, false}
      ]}
    ]},

    %% Char filter
    PhoneDigitsOnlyCharFilter = {<<"digits_only">>,
      [{<<"type">>, <<"pattern_replace">>},
        {<<"pattern">>, <<"[^\\d]">>}]},
    %% Filter
    PhoneNotEmptyFilter = {<<"not_empty">>, [{<<"type">>, <<"length">>}, {<<"min">>, 1}]},
    TrigramsFilter = {<<"trigrams_filter">>, [{<<"type">>, <<"ngram">>}, {<<"min_gram">>, 3}, {<<"max_gram">>, 3}]},
    %% Analyzers
    PhoneNumberAnalyzer = {<<"phone_number">>,
      [{<<"char_filter">>, <<"digits_only">>},
        {<<"tokenizer">>, <<"keyword">>},
        {<<"filter">>, []}]},
    PhoneNumberSearchAnalyzer = {<<"phone_number_search">>,
      [{<<"char_filter">>, <<"digits_only">>},
        {<<"tokenizer">>, <<"keyword">>},
        {<<"filter">>, [<<"not_empty">>]}]},
    TrigramsAnalyzer = {<<"trigrams">>, [
      {<<"type">>, <<"custom">>},
      {<<"tokenizer">>, <<"standard">>},
      {<<"filter">>, [
        <<"lowercase">>,
        <<"trigrams_filter">>
      ]}
    ]},

    Settings = [
      {<<"index">>, [
        {<<"blocks">>, [
          {<<"read_only_allow_delete">>, false}
        ]}
      ]},
      {<<"analysis">>,
        [
          {<<"char_filter">>, [PhoneDigitsOnlyCharFilter]},
          {<<"filter">>, [PhoneNotEmptyFilter, TrigramsFilter]},
%%            {<<"analyzer">>, [PhoneNumberAnalyzer, PhoneNumberSearchAnalyzer, TrigramsAnalyzer]}
          {<<"analyzer">>, [TrigramsAnalyzer]}
        ]}
    ],

    Mapping = [{
      <<"properties">>, [
        {<<"id">>, [{<<"type">>, <<"keyword">>}]},
        {<<"name">>, [
          {<<"type">>, <<"text">>},
          {<<"fields">>, [
            {<<"en">>, [
              {<<"type">>, <<"text">>},
              {<<"analyzer">>, <<"english">>}
            ]},
            {<<"pt">>, [
              {<<"type">>, <<"text">>},
              {<<"analyzer">>, <<"portuguese">>}
            ]},
            {<<"ru">>, [
              {<<"type">>, <<"text">>},
              {<<"analyzer">>, <<"russian">>}
            ]},
            {<<"th">>, [
              {<<"type">>, <<"text">>},
              {<<"analyzer">>, <<"thai">>}
            ]},
            {<<"general">>, [
              {<<"type">>, <<"text">>}
            ]}
          ]}]},
        {<<"username">>, [{<<"type">>, <<"keyword">>}]},
        {<<"skype">>, [{<<"type">>, <<"keyword">>}]},
        {<<"email">>, [{<<"type">>, <<"keyword">>}]},
        {<<"phone">>, [
          {<<"type">>, <<"text">>}
%%            {<<"analyzer">>, <<"phone_number">>},
%%            {<<"search_analyzer">>, <<"phone_number_search">>}
        ]},
        {<<"facebookId">>, [{<<"type">>, <<"keyword">>}]},
        {<<"vkontakteId">>, [{<<"type">>, <<"keyword">>}]},
        {<<"createdAt">>, [{<<"type">>, <<"date">>}]},
        {<<"updatedAt">>, [{<<"type">>, <<"date">>}]},
        {<<"isBot">>, [{<<"type">>, <<"boolean">>}]},
        {<<"excludeMe">>, [{<<"type">>, <<"boolean">>}]},
        {<<"photo">>, [{<<"type">>, <<"text">>}]},
        {<<"thumbnail">>, [{<<"type">>, <<"text">>}]},
        {<<"vendorId">>, [{<<"type">>, <<"keyword">>}]},
        {<<"isVendor">>, [{<<"type">>, <<"boolean">>}]},
        {<<"bio">>, [{<<"type">>, <<"text">>}]}
      ]}],
    R1 = case index_exists(?ELASTIC_USER_IDX) of
      false ->
        UserIndexData = [
          {<<"settings">>, Settings},
          {<<"mappings">>, [{?USER_DOC, Mapping}]}
        ],
        erlastic_search:create_index(?ELASTIC_USER_IDX, UserIndexData);
      true ->
        erlastic_search:put_setting(?ELASTIC_USER_IDX, Settings),
        erlastic_search:put_mapping(?ELASTIC_USER_IDX, ?USER_DOC, Mapping)
    end,

    case R1 of
      {error, Reason1} -> throw(Reason1);
      {ok, _} -> skip
    end,

    im_logger:debug(undefined, "[Elastica] ensure_index result - ~p", [R1]),

    % R2 = case index_exists(?ELASTIC_LOG_IDX) of
    %   false -> erlastic_search:create_index(?ELASTIC_LOG_IDX, [{<<"settings">>, [AllowRWSetting]}]);
    %   true  -> erlastic_search:put_setting(?ELASTIC_LOG_IDX, [AllowRWSetting])
    % end,
    % case R2 of
    %   {error, Reason2} -> throw(Reason2);
    %   {ok, _}          -> skip
    % end,
    % [R1, R2]

    [R1]
  end,

  try Fun() catch
    Error:Reason ->
      im_logger:error(undefined, "[Elastica] ensure_index terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end.

upsert_user(U) ->
  Fun = fun() ->
    Id = im_common:format_id(U#'im_usr'.id),

    Document = [
      {<<"id">>, Id},
      {<<"name">>, format_value(im_common:ensure_binary(U#'im_usr'.name))},
      {<<"username">>, format_value(im_common:ensure_binary(U#'im_usr'.username))},
      {<<"skype">>, format_value(im_common:ensure_binary(U#'im_usr'.skype))},
      {<<"email">>, format_value(im_common:ensure_binary(U#'im_usr'.email))},
      {<<"phone">>, format_value(im_common:ensure_binary(U#'im_usr'.phone))},
      {<<"facebookId">>, format_value(im_common:ensure_binary(U#'im_usr'.facebookId))},
      {<<"vkontakteId">>, format_value(im_common:ensure_binary(U#'im_usr'.vkontakteId))},
      {<<"createdAt">>, format_value(im_common:ensure_integer(U#'im_usr'.createdAt))},
      {<<"updatedAt">>, format_value(im_common:ensure_integer(U#'im_usr'.updatedAt))},
      {<<"isBot">>, format_value(im_common:ensure_boolean(U#'im_usr'.isBot))},
      {<<"excludeMe">>, format_value(im_common:ensure_boolean(U#'im_usr'.excludeMe))},
      {<<"photo">>, format_value(im_common:ensure_binary(U#'im_usr'.photo))},
      {<<"thumbnail">>, format_value(im_common:ensure_binary(U#'im_usr'.thumbnail))},
      {<<"isVendor">>, format_value(im_common:ensure_boolean(U#'im_usr'.isVendor))},
      {<<"vendorId">>, format_value(im_common:format_id(U#'im_usr'.vendorId))},
      {<<"bio">>, format_value(im_common:ensure_binary(U#'im_usr'.bio))}
    ],
    erlastic_search:upsert_doc(?ELASTIC_USER_IDX, ?USER_DOC, Id, Document)
  end,

  try Fun() catch
    Error:Reason ->
      im_logger:error(undefined, "[Elastica] upsert_user terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end.

reindex_users() ->
  reindex_users(ctail:all(im_usr)).

reindex_users([]) -> ok;
reindex_users([User | T]) ->
  case upsert_user(User) of
    {ok, _} ->
      im_logger:debug(undefined, "[Elastica] reindex_users ~p - ok", [im_common:format_id(User#'im_usr'.id)]),
      ok;
    {error, Reason} ->
      im_logger:error(undefined, "[Elastica] reindex_users ~p - failed with reason: ~p", [im_common:format_id(User#'im_usr'.id), Reason])
  end,
  reindex_users(T).

drop_and_reindex_users() ->
  Fun = fun() ->
    im_logger:debug(undefined, "[Elastica] drop_and_reindex_users", []),
    case index_exists(?ELASTIC_USER_IDX) of
      true ->
        im_logger:debug(undefined, "[Elastica] drop_and_reindex_users delete index - ~p.", [?ELASTIC_USER_IDX]),
        erlastic_search:delete_index(?ELASTIC_USER_IDX);
      false -> skip
    end,
    case index_exists(?ELASTIC_LOG_IDX) of
      true ->
        im_logger:debug(undefined, "[Elastica] drop_and_reindex_users delete index - ~p.", [?ELASTIC_LOG_IDX]),
        erlastic_search:delete_index(?ELASTIC_LOG_IDX);
      false -> skip
    end,
    ensure_index(),
    reindex_users()
  end,

  try Fun() catch
    Error:Reason ->
      im_logger:error(undefined, "[Elastica] upsert_user terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end.

find_users_by_query(QueryString, FindOnlyVendors, UserIds, UserId) ->
  IsAdmin = im_acl:has_role("super_admin", UserId),

  case FindOnlyVendors =:= true andalso not IsAdmin of
    true ->
      {error, ?ERROR_CODE_PERMISSION_DENIED};
    false ->
      QueryStringFormatted = case QueryString of undefined -> ""; _ -> QueryString end,
      QueryStringFormattedAsList = im_common:trim(im_common:ensure_list(QueryStringFormatted)),

      case string:len(QueryStringFormattedAsList) >= ?MIN_SEARCH_WORLD_LENGTH of
        true ->
          ShouldItems = [
            [{<<"multi_match">>, [
              {<<"query">>, QueryStringFormatted},
              {<<"fields">>, [<<"username">>, <<"name">>]},
              {<<"type">>, <<"phrase_prefix">>},
              {<<"minimum_should_match">>, <<"100%">>}
            ]}],
            [{<<"match">>, [
              {<<"name">>, [
                {<<"query">>, QueryStringFormatted},
                {<<"fuzziness">>, 1}
              ]}
            ]}]
          ],

          FormattedPhone = re:replace(QueryStringFormattedAsList, "[^0-9]", "", [global, {return, list}]),
          Should = case IsAdmin andalso length(FormattedPhone) >= ?MIN_SEARCH_PHONE_LENGTH of
            true ->
              PhoneQs = [{<<"wildcard">>, [
                {<<"phone">>, list_to_binary("*" ++ FormattedPhone ++ "*")}
              ]}],
              ShouldItems ++ [PhoneQs];
            false ->
              ShouldItems
          end,

          MustNot = case IsAdmin of
            true -> [
              [{<<"term">>, [{<<"id">>, im_common:format_id(UserId)}]}]
            ];
            false -> [
              [{<<"term">>, [{<<"excludeMe">>, true}]}],
              [{<<"term">>, [{<<"id">>, im_common:format_id(UserId)}]}]
            ]
          end,

          Bool1 = case UserIds of
            [] ->
              [{<<"must_not">>, MustNot}, {<<"should">>, Should}];
            _ ->
              Must = [[{<<"terms">>, [{<<"id">>, lists:map(fun(I) -> im_common:format_id(I) end, UserIds)}]}]],
              [{<<"must_not">>, MustNot}, {<<"must">>, Must}]
          end,
          Bool2 = case FindOnlyVendors =:= true of
            true ->
              Bool1 ++ [{<<"filter">>, [{<<"term">>, [{<<"isVendor">>, true}]}]}];
            false ->
              Bool1
          end,
          Bool = [{<<"bool">>, Bool2}],
          Query = [{<<"query">>, Bool}],

          io:format("~n~nQueryString ~p~n~n", [QueryString]),
          io:format("~n~nQuery ~p~n~n", [Query]),
          io:format("~n~nShould ~p~n~n", [Should]),
          io:format("~n~nShould ~p~n~n", [jsx:encode(Should)]),

          case erlastic_search:search(?ELASTIC_USER_IDX, ?USER_DOC, Query) of
            {ok, Response} ->
              [
                {<<"took">>, _Took},
                {<<"timed_out">>, _Timeout},
                {<<"_shards">>, _Shards},
                {<<"hits">>, [
                  {<<"total">>, _Total},
                  {<<"max_score">>, _MaxScore},
                  {<<"hits">>, Hits}
                ]}
              ] = Response,

              Records = lists:map(fun(Record) ->
                [
                  {<<"_index">>, _Index},
                  {<<"_type">>, _Type},
                  {<<"_id">>, _Id},
                  {<<"_score">>, _Score},
                  {<<"_source">>, Source}
                ] = Record,

                #'ContactEntity'{
                  userId = parse_value(proplists:get_value(<<"id">>, Source)),
                  name = parse_value(proplists:get_value(<<"name">>, Source)),
                  username = parse_value(proplists:get_value(<<"username">>, Source)),
                  phone = parse_value(proplists:get_value(<<"phone">>, Source)),
                  originalPhone = parse_value(proplists:get_value(<<"phone">>, Source)),
                  email = parse_value(proplists:get_value(<<"email">>, Source)),
                  skype = parse_value(proplists:get_value(<<"skype">>, Source)),
                  isBot = parse_value(proplists:get_value(<<"isBot">>, Source)),
                  photo = parse_value(proplists:get_value(<<"photo">>, Source)),
                  thumbnail = parse_value(proplists:get_value(<<"thumbnail">>, Source)),
                  isVendor = parse_value(proplists:get_value(<<"isVendor">>, Source)),
                  bio = parse_value(proplists:get_value(<<"bio">>, Source))
                }
              end, Hits),
              {ok, Records};
            {error, Reason} ->
              {error, ?ERROR_CODE_UNKNOWN}
          end;
        false ->
          {error, ?ERROR_CODE_USER_NAME_REQUIRED}
      end
  end.

index_exists(Name) ->
  Fun = fun() ->
    case erlastic_search:index_exists(Name) of
      {ok, IndexExists} -> IndexExists;
      {error, _} -> false
    end
  end,

  try Fun() catch
    Error:Reason ->
      im_logger:error(undefined, "[Elastica] index_exists terminated: ~p. Reason: ~p", [Error, Reason]),
      {error, Reason}
  end.

parse_value(null) -> undefined;
parse_value(Value) -> Value.

format_value(undefined) -> null;
format_value(Value) -> Value.
