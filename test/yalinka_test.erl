-module(yalinka_test).

-include_lib("eunit/include/eunit.hrl").

-define(POINTS, [{0,  10.0, 10.0, 10.0},
                 {1,  10.0, 10.0,-10.0},
                 {2,  10.0,-10.0, 10.0},
                 {3,  10.0,-10.0,-10.0},
                 {4, -10.0, 10.0, 10.0},
                 {5, -10.0, 10.0,-10.0},
                 {6, -10.0,-10.0, 10.0},
                 {7, -10.0,-10.0,-10.0}]).

-define(ROOT, [{11, 10.0,10.0,10.0}, {12, -10.0,-10.0,-10.0}, {13, 1.0,1.0,1.0}]).

size_test_() ->
    {ok, Tree} = yalinka:new(?POINTS),
    Size = yalinka:size(Tree),
    [?_assertEqual(Size, {ok, length(?POINTS)})].

clear_teest_() ->
    {ok, Tree} = yalinka:new(?POINTS),
    Clear = yalinka:clear(Tree),
    [?_assertEqual(Clear, ok)].

root_test_() ->
    {ok, Ref} = yalinka:new(?ROOT),
    [
     ?_assertEqual( yalinka:root(Ref), {ok,13,{1.0,1.0,1.0}} )
    ].

search_test_() ->
    {ok, Tree} = yalinka:new(?POINTS),
    [
     ?_assertEqual( {ok, [{0, 0.0}]}, yalinka:search(Tree, {10.0, 10.0, 10.0}, 1)),
     ?_assertEqual( {ok, [{0, 1.0}]}, yalinka:search(Tree, {10.0, 10.0, 11.0}, 1)),
     ?_assertEqual( {ok, [{2, 200.0}, {0, 200.0}, {4, 200.0}, {6, 200.0}]},
                    yalinka:search(Tree, {0.0, 0.0, 10.0}, 4)),
     ?_assertEqual( {ok, [{0, 50.0}, {2, 250.0}, {4, 250.0}]},
                    yalinka:search(Tree, {5.0, 5.0, 10.0}, 3) )
    ].

     
     
