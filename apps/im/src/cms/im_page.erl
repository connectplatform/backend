-module(im_page).
-include("im_common.hrl").
-export([get/1]).

get(#'Pages'{ref=Ref}) ->
  PageItems = ctail:all(im_page),
  Sorted = lists:sort(fun(A, B) -> A#im_page.title > B#im_page.title end, PageItems),
  Pages = lists:map(fun (X) -> im_dto:format_page(X) end, Sorted),
  #'PagesResp'{ref=Ref, pages=Pages}.
