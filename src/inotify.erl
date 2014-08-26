%%%-------------------------------------------------------------------
%%% Created : 28 Jan 2010 by Mats Cronqvist <masse@kreditor.se>
%%% Converted to an OTP application by Sven Heyll <sven.heyll@gmail.com>
%%%
%%% @doc
%%% Application that monitors files via Linux `inotify'. A user can tell
%%% `inotify' to {@link watch/2} for specific events on several
%%% files/directories.  To actually get any events the User must call {@link
%%% add_handler/3}.
%%%
%%% To {@link unwatch/1} something the User must pass the return value of {@link
%%% watch/2} which is a unique reference to the monitor create by {@link
%%% watch/2}.
%%%
%%% This module also defines a behaviour, with a single function
%%% `inotify_event(Arg, EventRef, Msg)'
%%%
%%% A callback for file monitoring event handler added by {@link add_handler/3}.
%%%
%%% `Arg' is the user provided extra argument.
%%%
%%% `EventRef' is the value returned by {@link watch/2}.
%%%
%%% The `Msg' parameter of the callback function should be pattern-matched
%%% with the macro `?inotify_msg(Mask, Cookie, OptionalName)' contained
%%% in `include/inotify.hrl'.
%%%
%%% `Mask' is a list of atoms describing the nature of the event. Refer to the
%%% Linux inotify man page for a detailed description.
%%%
%%% `Cookie' is 0 except when a file is moved, where it is used to identify
%%% `move_from' and `move_to' events belonging to the same move-operation.
%%%
%%% `OptionalName' contains the relative file name when monitoring whole
%%% directories. E.g. when monitoring "/tmp" the following event might be
%%% created when opening "/tmp/xxx": `?inotify_msg([open], 0, "xxx")'.
%%% @end
%%%-------------------------------------------------------------------

-module(inotify).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2,
         watch/1, watch/2, unwatch/1, add_handler/3, print_events/1]).

%% Supervisor callbacks
-export([init/1]).

%% inotify_evt callbacks
-export([inotify_event/3]).

-export_type([mask/0, msg/0]).

-define(SERVER, inotify_server).
-define(EVENT, inotify_evt).

-include("inotify.hrl").

-type msg() :: ?inotify_msg(Mask :: [mask()],
                            Cookie :: non_neg_integer(),
                            OptionalName :: string()).
%% A file monitoring message.

-type mask() :: ?ALL           |
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
%% The type of a file monitoring event.

%%%===================================================================
%%% Behaviour definition
%%%===================================================================

%%--------------------------------------------------------------------
%% FUCK YOU EDOC!
%%--------------------------------------------------------------------
-callback inotify_event(Arg :: term(),
                        EventRef :: term(),
                        Msg :: inotify:msg()) ->
    ok | remove_handler.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Add a file/directory monitor for all file events. Events for the
%% file/directory will be published via a {@link inotify_evt}. The events
%% resulting from this call will be identified by the unique reference returned
%% by this function, no matter if the operation was successful or not.
%%
%% This reference must also be passed to {@link unwatch/1}.
%% @see inotify:add_handler/3
%% @see inotify:unwatch/1
%% @end
%%--------------------------------------------------------------------
-spec watch(string()) ->
                   reference().
watch(File) ->
    watch(File, ?ALL).

%%--------------------------------------------------------------------
%% @doc
%% Add a file/directory monitor for specific events.  This differs from {@link
%% watch/2} by the additional `mask' argument.  Calling {@link watch/2} is
%% equivalant to calling {@link watch/3} with `?ALL' as second parameter.
%%
%% @see inotify:watch/1
%% @end
%%--------------------------------------------------------------------
-spec watch(string(), mask() | [mask()]) ->
                   reference().
watch(File, Mask) ->
    EventTag = make_ref(),
    inotify_server:watch(File, EventTag, Mask),
    EventTag.

%%--------------------------------------------------------------------
%% @doc
%% Remove a monitor added via {@link watch/2}, `Ref' identifies the
%% monitor.
%% @end
%%--------------------------------------------------------------------
-spec unwatch(reference()) ->
                     ok.
unwatch(Ref) ->
    inotify_evt:remove_all_handlers(Ref),
    inotify_server:unwatch(Ref).

%%--------------------------------------------------------------------
%% @doc
%% Add an event handler that receives all events generated by a file monitor
%% identified by a tag that was passed to {@link inotify:watch/2}. When an event
%% occurs for `Ref' the handler will call `Module:inotify_event(Arg,
%% EventTag, Msg)' as defined by the `callback' definition in {@link
%% inotify_event/3}.
%% @end
%%--------------------------------------------------------------------
-spec add_handler(reference(), module(), term()) ->
                         ok.
add_handler(Ref, Module, Arg) ->
    inotify_evt:add_handler(Ref, Module, Arg).

%%--------------------------------------------------------------------
%% @doc
%% Add an event handler that prints to stdout events. The events to print are
%% identified by a reference returned by {@link watch/2}.
%% @end
%%--------------------------------------------------------------------
-spec print_events(reference()) ->
                          ok.
print_events(Ref) ->
    inotify_evt:add_handler(Ref, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% use inotify as a lib without process
%% @end
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(ServerOpts, EventOpts) ->
    Ret = supervisor:start_link(?MODULE,
                                [{server, ServerOpts}, {event, EventOpts}]),
    Ret.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init(Opts) ->
    EventOpts  = proplists:get_value(event, Opts, []),
    ServerOpts = proplists:get_value(server, Opts, []),
    {ok, {{one_for_all, 1, 1},
          [{?EVENT,  {?EVENT, start_link, EventOpts},
            permanent, 2000, worker, [?EVENT]},
           {?SERVER, {?SERVER, start_link, ServerOpts},
            permanent, 2000, worker, [?SERVER]}]}}.

%%%===================================================================
%%% inotify_evt callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This module actually implements the {@link inotify_evt} behaviour, with a
%% function that simply prints the events to stdout.
%%
%% This function can be used as an example for how to implement a callback.
%%
%% @see print_events/1
%% @end
%%--------------------------------------------------------------------
inotify_event([], Ref, ?inotify_msg(Masks, Cookie, OptionalName)) ->
    io:format("[INOTIFY] - ~p - ~p ~p ~p~n", [Ref, Masks, Cookie, OptionalName]).
