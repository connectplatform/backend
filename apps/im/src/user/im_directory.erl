-module(im_directory).

-include("im_common.hrl").

-export([is_turnedon/0]).
-export([list/1, c/1, r/1, u/1, d/1]).
-export([update_directory/2, spawn_contacts/2]).
-export([update_directory_status/0, update_contacts/0, update_directory_avatar/1]).

is_turnedon() ->
  case sm:env(im, use_directory, false) of
    true  -> true;
    false -> false;
    _     -> false
  end.

list({Skip, Limit, QueryString}) ->
  QueryString1 = im_common:ensure_binary(QueryString),
  Query = #{
    <<"$or">> => [
      #{<<"phone">> => #{<<"$regex">> => QueryString1, <<"$options">> => <<"i">>}},
      #{<<"email">> => #{<<"$regex">> => QueryString1, <<"$options">> => <<"i">>}},
      #{<<"name">>  => #{<<"$regex">> => QueryString1, <<"$options">> => <<"i">>}}
    ]
  },
  Order = #{
    <<"$orderby">> => #{
      <<"name">> => 1,
      <<"blocked">> => 1
    }
  },

  Total = ctail_mongo:exec(count, [<<"im_directory">>, Query]),

  Resp = [ format_directory(D) || D <- ctail_mongo:find(im_directory, maps:merge(#{<<"$query">> => Query}, Order), Skip, Limit)],

  {Total, Resp}.

c({Name, Phone, Email, DepartmentId}) ->
  FormattedPhone = purify_phone(Phone),

  case ctail:get(im_directory, FormattedPhone) of
    {ok, _} -> {err, <<"Error">>};
    _ ->
      {ok, NewUser} = im_auth:ensure_user_by_phone(FormattedPhone),
      ctail:put(im_user_transform:serialize(NewUser#im_usr{name=Name, departmentId=DepartmentId})),

      DirUnit = #im_directory{
        id            = FormattedPhone, %% add im_directory_phone_lookup
        userId        = NewUser#im_usr.id,
        name          = Name,
        email         = Email,
        phone         = FormattedPhone,
        departmentId  = DepartmentId,
        blocked       = false
      },
      ok = ctail:put(DirUnit),

      Contact = dirunit_to_contact(DirUnit),
      Upload = #'UploadContacts'{contacts=[Contact]},

      Users = ctail:all(im_usr),
      [im_contact_api:upload(Upload, User, #im_usr_token{}) || User <- Users, User#im_usr.phone =/= ?APPLE_USER_PHONE],

      {ok, FormattedPhone}
  end.

r({Phone}) ->
  case ctail:get(im_directory, Phone) of
    {ok, Directory} -> format_directory(Directory);
    _               -> {err, <<"Error">>}
  end.

u({UserId, Name, Phone, Email, DepartmentId}) ->
  FormattedPhone = purify_phone(Phone),
  {ok, DirUnit} = ctail:get(im_directory, UserId),
  Updated = DirUnit#im_directory{
    name=Name,
    email=Email,
    phone=FormattedPhone,
    departmentId=DepartmentId
  },
  ok = ctail:put(im_user_transform:serialize(Updated)),

  case im_roster_chat:get(DirUnit#im_directory.userId) of
    undefined -> skip;
    IdUser ->
      UpdatedUser = IdUser#im_usr{
        name         = Name,
        departmentId = DepartmentId,
        updatedAt    = sm:now()
      },
      ctail:put(im_user_transform:serialize(UpdatedUser)),

      im_contact:update_contacts_when_user_changed(UpdatedUser),

      Msg = #'ProfileChanged'{user=im_dto:format_user(UpdatedUser)},
      im_user_state:broadcast(undefined, [UpdatedUser#im_usr.id], Msg)
  end,

  ctail_mongo:exec(find_one, [to_binary(im_directory), {<<"_id">>, make_id(UserId)}]).

d({UserId}) ->
  {ok, DirUnit} = ctail:get(im_directory, UserId),
  case DirUnit#im_directory.blocked of
    true  ->
      ok = ctail:put(DirUnit#im_directory{blocked=false}),
      {ok, {}} = im_usr:unblock_user_by_system(#'UnBlockUserBySystem'{userId=im_common:format_id(DirUnit#im_directory.userId)});
    false ->
      ok = ctail:put(DirUnit#im_directory{blocked=true}),
      {ok, {}} = im_usr:block_user_by_system(#'BlockUserBySystem'{userId=im_common:format_id(DirUnit#im_directory.userId)})
  end,
  ok.

update_directory(UserId, FormattedPhone) ->
  case ctail:get(im_directory, FormattedPhone) of
    {ok, DirUnit} ->
      DirUnit1 = DirUnit#im_directory{userId=UserId, isNew=false},
      ok = ctail:put(DirUnit1);
    _             -> skip
  end.

update_directory_status() ->
  [ begin
      case im_roster_chat:get(Directory#im_directory.userId) of
        undefined -> ctail:put(Directory#im_directory{isNew=false});
        User -> ctail:put(Directory#im_directory{isNew=User#im_usr.isNew})
      end
    end || Directory <- ctail:all(im_directory)].

spawn_contacts(UserId, DeviceId) ->
  WholeDirectory = ctail:all(im_directory),
  Directory = lists:filter(fun(_DirUnit=#im_directory{blocked=Blocked}) ->
                             not Blocked
                           end, WholeDirectory),
  Contacts = lists:map(fun dirunit_to_contact/1, Directory),
  Upload = #'UploadContacts'{contacts=Contacts},

  im_contact_api:upload(Upload, #im_usr{id=UserId}, #im_usr_token{deviceId=DeviceId}).

purify_phone(Phone) ->
  {ok, CountryCode} = im_common:get_county_code(Phone),
  list_to_binary("+" ++ integer_to_list(CountryCode) ++ im_common:format_phone(Phone)).

dirunit_to_contact(_DirUnit=#im_directory{
  userId=UserId1,
  name=Name,
  email=Email,
  phone=Phone,
  photo=Photo,
  departmentId=DepartmentId,
  thumbnail=Thumbnail}) ->
  #'ContactEntity'{
    userId=UserId1,
    name=Name,
    email=Email,
    phone=[Phone],
    photo=Photo,
    thumbnail=Thumbnail,
    departmentId=DepartmentId,
    status=?CONTACT_STATUS_FRIEND
  }.

to_binary({<<ObjectId:12/binary>>}) -> {ObjectId};
to_binary({Key, Value})             -> {Key, to_binary(Value)};
to_binary(Value)                    -> to_binary(Value, false).

to_binary(Value, ForceList) ->
  if
    is_integer(Value) ->
      Value;
    is_list(Value) ->
      unicode:characters_to_binary(Value, utf8, utf8);
    is_atom(Value) ->
      atom_to_binary(Value, utf8);
    true ->
      case ForceList of
        true ->
          [List] = io_lib:format("~p", [Value]),
          list_to_binary(List);
        _ ->
          Value
      end
  end.

make_id({<<ObjectId:12/binary>>}) -> {ObjectId};
make_id(Term)                     -> to_binary(Term, true).

format_directory(
    #im_directory{
      id=Id,
      userId=UserId,
      name=Name,
      email=Email,
      phone=Phone,
      thumbnail=Thumbnail,
      photo=Photo,
      departmentId=DepartmentId,
      blocked=Blocked,
      isNew=IsNew
    }
) ->
  #'DirectoryEntity'{
    id              = Id,
    userId          = im_common:format_id(UserId),
    name            = im_common:format_utf8(Name),
    email           = im_common:format_utf8(Email),
    phone           = im_common:format_utf8(Phone),
    photo           = im_common:format_utf8(Photo),
    thumbnail       = im_common:format_utf8(Thumbnail),
    departmentId    = im_common:format_id(DepartmentId),
    isNew           = IsNew,
    blockedBySystem = Blocked
  }.

update_directory_avatar(#im_usr{phone=Phone, thumbnail=Thumbnail, photo=Photo}) ->
  case ctail:get(im_directory, Phone) of
    {ok, #im_directory{thumbnail=Thumbnail, photo=Photo}} -> skip;
    {ok, DirUnit} ->
      DirUnit1 = DirUnit#im_directory{photo=Photo, thumbnail=Thumbnail},
      ok = ctail:put(DirUnit1);
    _ -> skip
  end.

update_contacts() ->
  WholeDirectory = ctail:all(im_directory),
  Directory = lists:filter(fun(_DirUnit=#im_directory{blocked=Blocked}) ->
    not Blocked
                           end, WholeDirectory),

  Contacts = lists:map(fun dirunit_to_contact/1, Directory),

  Upload = #'UploadContacts'{contacts=Contacts},
  Users = ctail:all(im_usr),

  [im_contact_api:upload(Upload, #im_usr{id=User#im_usr.id}, #im_usr_token{})
    || User <- Users, User#im_usr.phone =/= ?APPLE_USER_PHONE].
