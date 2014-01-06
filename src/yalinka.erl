%%%-------------------------------------------------------------------
%%% @author Serge A. Ribalchenko <fisher@heim.in.ua>
%%% @copyright (C) 2013, Serge A. Ribalchenko
%%% @doc
%%%      main API file
%%% @end
%%% Created : 28 Dec 2013 by Serge A. Ribalchenko <fisher@heim.in.ua>
%%%-------------------------------------------------------------------
-module(yalinka).

-on_load(init/0).


-export([new/1, search/3, size/1, dimension/1, root/1, node/2, clear/1]).

-export([list_return/1, test/1, revlist/1, normalize_to_bin/1, getting_proplist/1]).


-type point() :: {integer(), float(), float(), float()}.


-define(not_loaded, erlang:nif_error(not_loaded)).


list_return(_) ->
    ?not_loaded.
test(_) ->
    ?not_loaded.
revlist(_) ->
    ?not_loaded.
normalize_to_bin(_) ->
    ?not_loaded.
getting_proplist(_) ->
    ?not_loaded.

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc create new k-d tree object
%% @end
%%--------------------------------------------------------------------
-spec new([point()]) -> {ok, reference()}.
new(_Points) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec search(reference(), {float(), float(), float()}, integer()) -> 
                    [{integer(), float()}].
search(_Ref, _Point, _K) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return the quantity of a nodes in the tree
%% @end
%%--------------------------------------------------------------------
-spec size(reference()) -> integer().
size(_Ref) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return space dimension (arity of the point in space)
%% @end
%%--------------------------------------------------------------------
-spec dimension(reference()) -> integer().
dimension(_Ref) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return the root of the tree
%% @end
%%--------------------------------------------------------------------
-spec root(reference()) -> point().
root(_Ref) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return the specified node of the unsorted tree
%% @end
%%--------------------------------------------------------------------
-spec node(reference(), integer()) -> point().
node(_Ref, _Idx) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec clear(reference()) -> ok.
clear(_Ref) ->
    ?not_loaded.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc module initialization
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | {error, any()}.
init() ->
    NifLocation =
	filename:join( privplace(), "lib" ),
    erlang:load_nif(
      filename:join(NifLocation, "yalinka"), 0).

%%--------------------------------------------------------------------
%% @doc find out the path to privdir of the current erlang application
%% @end
%%--------------------------------------------------------------------
-spec privplace() -> string().
privplace() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppPath = filename:dirname(EbinDir),
    filename:join(AppPath, "priv").
