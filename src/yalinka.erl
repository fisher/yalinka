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


-export([
         new/1, search/2, search/3,
         size/1, dimension/1, root/1, node/2, clear/1]).


-type point() :: {integer(), float(), float(), float()}.


-define(not_loaded, erlang:nif_error(not_loaded)).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc create new k-d tree object
%% @end
%%--------------------------------------------------------------------
-spec new([point()]) -> {ok, Tree::reference()} | {error, Reason::term()}.
new(_Points) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc search for nearest point in the Tree to the given Point
%% @end
%%--------------------------------------------------------------------
-spec search(reference(), {float(), float(), float()}) ->
                    {Idx::integer(), Distance::float()}.
search(_Tree, _Point) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc find first N nearest points.
%% @see search/2
%% @end
%%--------------------------------------------------------------------
-spec search(reference(), {float(), float(), float()}, integer()) -> 
                    [{Idx::integer(), Distance::float()}].
search(_Tree, _Point, _N) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return the quantity of a nodes in the Tree
%% @end
%%--------------------------------------------------------------------
-spec size(reference()) ->
                  {ok, Size::integer()} | {error, Reason::term()}.
size(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return space dimension (arity of the point in space) for the
%%     given Tree
%% @end
%%--------------------------------------------------------------------
-spec dimension(reference()) ->
                       {ok, Dimension::integer()} | {error, Reason::term()}.
dimension(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @private
%% @doc return the root of the Tree
%% @end
%%--------------------------------------------------------------------
-spec root(reference()) -> point().
root(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @private
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
-spec init() -> ok | {error, Reason::term()}.
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
