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
         new/1, search/2, search/3, gettree/1, store/2, load/1,
         size/1, dimension/1, root/1, node/2, clear/1]).

-export_type([point/0, tag/0, tnode/0, tref/0]).

-type tag() :: Idx::integer().

-type point() :: [float()] | tuple(float()).

-type tnode() :: {Tag::tag(), Point::point()}.

-type tref() :: reference().


-define(not_loaded, erlang:nif_error(not_loaded)).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc create new k-d tree object
%% @end
%%--------------------------------------------------------------------
-spec new([tnode()]) -> {ok, Tree::tref()} | {error, Reason::term()}.
new(_Points) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc search for nearest point in the Tree to the given Point
%% @end
%%--------------------------------------------------------------------
-spec search(tref(), point()) ->
                    {Tag::tag(), Distance::float()}.
search(_Tree, _Point) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc find first N nearest points.
%% @see search/2
%% @end
%%--------------------------------------------------------------------
-spec search(tref(), point(), integer()) -> 
                    [{Idx::integer(), Distance::float()}].
search(_Tree, _Point, _N) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return the quantity of a nodes in the Tree
%% @end
%%--------------------------------------------------------------------
-spec size(tref()) ->
                  {ok, Size::integer()} | {error, Reason::term()}.
size(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc export the Tree into erlang Term
%% @end
%%--------------------------------------------------------------------
-spec gettree(tref()) -> {ok, Term::[tnode()]} | {error, Reason::term()}.
gettree(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc return the space dimension (arity of the point in space) for
%%     the given Tree
%% @end
%%--------------------------------------------------------------------
-spec dimension(tref()) -> {ok, Dimension::integer()} |
                           {error, Reason::term()}.
dimension(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @private
%% @doc return the root of the Tree
%% @end
%%--------------------------------------------------------------------
-spec root(tref()) -> Node::tnode().
root(_Tree) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @private
%% @doc return the specified node of the unsorted tree
%% @end
%%--------------------------------------------------------------------
-spec node(tref(), integer()) -> Node::tnode().
node(_Tree, _Idx) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @private
%% @doc dummy. Now it is GC'ed
%% @end
%%--------------------------------------------------------------------
-spec clear(tref()) -> ok.
clear(_Ref) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc store the given Tree into specified File
%% @end
%%--------------------------------------------------------------------
-spec store(tref(), string()) -> ok | {error, Reason::term()}.
store(_Tree, _Filename) ->
    ?not_loaded.

%%--------------------------------------------------------------------
%% @doc load the Tree from specified Filename, previously stored with store/2.
%% @end
%%--------------------------------------------------------------------
-spec load(string()) -> {ok, Tree::tref()} | {error, Reason::term()}.
load(_Filename) ->
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
