-module(im_channel_post_worker).
-behaviour(gen_server).

-include("im_common.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-define(ETS_NAME, im_channel_post_sup).

-record(state, {id}).

start_link(PostId) -> gen_server:start_link(?MODULE, [PostId], []).

init([PostId]) ->
  process_flag(trap_exit, true),
  {ok, #state{id=PostId}}.

handle_call({like, PostId, UserId}, _, State)    ->
  {ok, Lookup} = ctail:get(channel_post_like_lookup, PostId),
  {ok, Post} = ctail:get(channel_post, PostId),

  NewLikesList = lists:usort([UserId|Lookup#channel_post_like_lookup.likes]),
  NewLookup = Lookup#channel_post_like_lookup{likes = NewLikesList},
  ctail:put(NewLookup),

  LikesCount = length(NewLikesList),

  LikesRecord = im_common:proplist_to_tuple(Post#channel_post.likes),
  NewLikesRecord = LikesRecord#'Likes'{count = LikesCount},
  NewPost = Post#channel_post{likes=im_common:tuple_to_proplist(NewLikesRecord)},
  ctail:put(NewPost),

  {reply, LikesCount, State};

handle_call({unlike, PostId, UserId}, _, State)  ->
  {ok, Lookup} = ctail:get(channel_post_like_lookup, PostId),
  {ok, Post} = ctail:get(channel_post, PostId),

  NewLikesList = lists:usort(lists:delete(UserId, Lookup#channel_post_like_lookup.likes)),
  NewLookup = Lookup#channel_post_like_lookup{likes = NewLikesList},
  ctail:put(NewLookup),

  LikesCount = length(NewLikesList),

  LikesRecord = im_common:proplist_to_tuple(Post#channel_post.likes),
  NewLikesRecord = LikesRecord#'Likes'{count = LikesCount},
  NewPost = Post#channel_post{likes=im_common:tuple_to_proplist(NewLikesRecord)},
  ctail:put(NewPost),

  {reply, LikesCount, State};

handle_call({add_comment, PostId, Comment}, _, State)  ->
  {ok, Thread} = ctail:get(channel_post_thread, PostId),
  {ok, Post} = ctail:get(channel_post, PostId),

  PrevComments = Thread#channel_post_thread.comments,
  NewComments = [Comment | PrevComments],
  NewThread = Thread#channel_post_thread{comments=NewComments},
  ctail:put(NewThread),

  ThreadRecord = im_common:proplist_to_tuple(Post#channel_post.thread),
  NewThreadRecord = ThreadRecord#'Thread'{count = length(NewComments)},
  NewPost = Post#channel_post{thread=im_common:tuple_to_proplist(NewThreadRecord)},
  ctail:put(NewPost),

  {reply, ok, State};

handle_call({comment, _UserId, _Text}, _, State) -> {reply, ok, State};
handle_call({share, _UserId}, _, State)   -> {reply, ok, State};

handle_call(_, _, State)  -> {reply, ok, State}.
handle_cast(_, State)     -> {noreply, State}.
handle_info(_, State)     -> {noreply, State}.
terminate(_Reason, State) ->
  im_ets:delete(?ETS_NAME, State#state.id),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

