-module('1563747005_elasticsearch_index').
-behaviour(im_migration).

-export([up/0, down/0]).

up() ->
%%  im_elastic_search:drop_and_reindex_users(),
  ok.

down() -> ok.
