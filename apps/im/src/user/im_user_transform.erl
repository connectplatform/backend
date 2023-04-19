-module(im_user_transform).

-export([spec/0, handle/3]).
-export([serialize/1, deserialize/1]).

serialize(User)   -> im_std_worker:call(user_transform, {serialize, User}).
deserialize(User) -> im_std_worker:call(user_transform, {deserialize, User}).

spec() ->
  UserTransformOptions = [
    {serializers, [fun im_device:format_devices/1, fun im_chatupdate:format/1]},
    {deserializers, [fun im_device:parse_devices/1, fun im_chatupdate:parse/1]}
  ],
  im_std_worker:spec(user_transform, UserTransformOptions, fun ?MODULE:handle/3).

handle(init, Options, _) ->
  State = #{
    serializers => proplists:get_value(serializers, Options),
    deserializers => proplists:get_value(deserializers, Options)
  },
  {ok, State};

handle(call, {serialize, User}, State=#{serializers:=Serializers}) ->
  Document = lists:foldl(fun(SerializerFun, User1) ->
    SerializerFun(User1)
  end, User, Serializers),
  {reply, Document, State};

handle(call, {deserialize, Document}, State=#{deserializers:=Deserializers}) ->
  User = lists:foldl(fun(DeserializerFun, Document1) ->
    DeserializerFun(Document1)
  end, Document, Deserializers),
  {reply, User, State};

handle(_, _, _) -> ok.
