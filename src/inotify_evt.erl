%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@sheyllpc>
%%% @copyright (C) 2013, Sven Heyll
%%% @doc
%%% Event manager for file monitoring events.
%%% This is an internal module.
%%% @end
%%% Created :  1 Jan 2013 by Sven Heyll <sven@sheyllpc>
%%%-------------------------------------------------------------------
-module(inotify_evt).

-behaviour(gen_event).

%% API
-export([start_link/0, publish/2, add_handler/3, remove_all_handlers/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("inotify.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
publish(EventTag, Msg) ->
    gen_event:notify(?MODULE, {?MODULE, EventTag, Msg}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
add_handler(EventTag, Module, Arg) ->
    gen_event:add_handler(?MODULE,
                          {?MODULE, Arg},
                          {EventTag, Module, Arg}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
remove_all_handlers(EventTag) ->
    gen_event:sync_notify(?MODULE, {remove_all_handlers, EventTag}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init({EventTag, Module, Arg}) ->
    {ok, {EventTag, Module, Arg}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_event({remove_all_handlers, EventTag},
             {EventTag, _Module, _Arg}) ->
    remove_handler;

handle_event({?MODULE, EventTag, Msg = ?inotify_msg(_, _, _)},
             State = {EventTag, Module, Arg}) ->
    case Module:inotify_event(Arg, EventTag, Msg) of
        ok ->
            {ok, State};
        remove_handler ->
            remove_handler
    end;

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
