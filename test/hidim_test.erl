
%%%-------------------------------------------------------------------
%%% @author Serge A. Ribalchenko <fisher@heim.in.ua>
%%% @copyright (C) 2014, Serge A. Ribalchenko
%%% @doc
%%%
%%% @end
%%% Created : 30 Apr 2014 by Serge A. Ribalchenko <fisher@heim.in.ua>
%%%-------------------------------------------------------------------
-module(hidim_test).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start() ->
    eunit:test([{module, ?MODULE}], [verbose]).


%%%===================================================================
%%% Eunit test functions
%%%===================================================================

first_test_() ->
    [
     ?_assert( true )
    ].

direct_load_test_() ->
    [
     ?_assertMatch( {module, _M}, code:load_file(yalinka))
    ].

pure_crash_test_() ->
    [
     ?_assertMatch( {ok, _Ref}, yalinka:new(
                                  [
                                   {2, 11.1,22.2,33.3,44.4},
                                   {3, 21.1,32.2,54.3,53.4}
                                  ])),
     ?_assertMatch( {ok, _Ref}, yalinka:new(
                                  [
                                   {2, 11.1,22.2,33.3,44.4},
                                   {3, 21.1,32.2,54.3,53.4}
                                  ]))
    ].

parallel_addition_test_() ->
    {ok,T} = yalinka:new([{0,0.0,0.0,0.0}]),
    [
     ?_assertEqual(ok, spawn_additions(T)),
     ?_assertEqual({ok, 708}, yalinka:size(T)),
     ?_assertMatch({ok, _List}, yalinka:gettree(T))
    ].

add_10_inarow(I,A,F,T)->
    spawn(
      fun()->
              [yalinka:insert(T,[{C,{float(C)+F,float(C)+F,float(C)+F}}])
               ||C<-lists:seq(I,I+100,A)]
      end).

spawn_additions(T) ->
    add_10_inarow(100,1,0.01,T),
    add_10_inarow(200,1,0.11,T),
    add_10_inarow(300,1,0.1,T),
    add_10_inarow(400,1,0.2,T),
    add_10_inarow(500,1,0.3,T),
    add_10_inarow(600,1,0.4,T),
    add_10_inarow(700,1,0.5,T),
    ok.
