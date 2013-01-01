%%%-------------------------------------------------------------------
%%% Created : 28 Jan 2010 by Mats Cronqvist <masse@kreditor.se>
%%% Converted to an OTP application by Sven Heyll <sven.heyll@gmail.com>
%%%
%%% @doc
%%% Application that monitors files via Linux `inotify'.
%%% @end
%%%-------------------------------------------------------------------

-module(inotify).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([watch/2, watch/3, unwatch/1, add_handler/3]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-export_type([mask/0, msg/0]).

-define(SERVER, inotify_server).
-define(EVENT, inotify_evt).

-include("inotify.hrl").

-type msg() :: ?inotify_msg([mask()], non_neg_integer(), string()).

-type mask() ::
        ?ALL           |
        ?ACCESS        |
        ?ATTRIB        |
        ?CLOSE_WRITE   |
        ?CLOSE_NOWRITE |
        ?CLOSE         |
        ?CREATE        |
        ?DELETE        |
        ?DELETE_SELF   |
        ?MODIFY        |
        ?MOVE_SELF     |
        ?MOVED_FROM    |
        ?MOVED_TO      |
        ?MOVE          |
        ?OPEN          |
        ?DONT_FOLLOW   |
        ?MASK_ADD      |
        ?ONLYDIR.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Add a monitor to inotify in the kernel. Events will be published via a {@link
%% inotify_evt}. The events resulting from this call will contain `EventTag'.
%% @end
%%--------------------------------------------------------------------
-spec watch(string(), term()) ->
                   ok.
watch(File, EventTag) ->
    inotify_server:watch(File, EventTag).

%%--------------------------------------------------------------------
%% @doc
%% Add a monitor to inotify in the kernel. Events will be published via a {@link
%% inotify_evt}. The events resulting from this call will contain `EventTag'.
%% This differs from {@link watch/2} by the additional `mask' argument.  Calling
%% {@link watch/2} is equivalant to calling {@link watch/3} with `?ALL' as third
%% parameter.
%% @end
%%--------------------------------------------------------------------
-spec watch(string(), term(), mask() | [mask()]) ->
                   ok.
watch(File, EventTag, Mask) ->
    inotify_server:watch(File, EventTag, Mask).

%%--------------------------------------------------------------------
%% @doc
%% Remove an inotify entry identified by its `EventTag' defined by {@link
%% watch/2}.
%% @end
%%--------------------------------------------------------------------
-spec unwatch(term()) ->
                     ok.
unwatch(EventTag) ->
    inotify_evt:unwatch(EventTag),
    inotify_server:unwatch(EventTag).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler for file alternation events identified by a tag that
%% was passed to {@link inotify:watch/2}. When an event occurs for `EventTag'
%% the handler will call `Module:inotify_event(Arg, EventTag, Msg)' as defined
%% by the `callback' definition in the module {@link inotify_evt}.
%% @end
%%--------------------------------------------------------------------
add_handler(EventTag, Module, Arg) ->
    inotify_evt:add_handler(EventTag, Module, Arg).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start(_StartType, _StartArg) ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
stop(_) ->
    ok.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
  {ok, {{one_for_all, 1, 1},
        [{?EVENT,  {?EVENT, start_link, []},
          permanent, 2000, worker, [?EVENT]},
         {?SERVER, {?SERVER, start_link, []},
          permanent, 2000, worker, [?SERVER]}]}}.
