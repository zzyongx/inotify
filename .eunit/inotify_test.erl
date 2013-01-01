%%%=============================================================================
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @copyright (C) 2013, Sven Heyll
%%%=============================================================================

-module(inotify_test).

-export([inotify_event/3]).

-include_lib("eunit/include/eunit.hrl").
-include("inotify.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

monitor_file_test() ->
    process_flag(trap_exit, true),
    file:delete("/tmp/inotify_test"),

    inotify:start(x,y),
    ok = inotify:watch("/tmp/", tag1, ?ALL),
    ok = inotify:add_handler(tag1, ?MODULE, self()),
    receive after 100 -> ok end,

    {ok, H} = file:open("/tmp/inotify_test", [read, write]),

    receive
        {[create],0,"inotify_test"} -> pass;
        M1 -> throw({expected_message_file_create_but_got, M1})
    after 1000 -> throw(expected_create)
    end,
    receive
        {[open],0,"inotify_test"} -> pass;
        M2 -> throw({expected_message_file_open_but_got, M2})
    after 1000 -> throw(expected_open)
    end,
    inotify:watch("/tmp/inotify_test", tag2),
    inotify:unwatch(tag1),
    ok = inotify:add_handler(tag2, ?MODULE, self()),
    receive after 100 -> ok end,
    io:write(H, "test"),
    receive
        {[modify],0,""} -> pass;
        M3 -> throw({expected_message_file_modify_but_got, M3})
    after 1000 -> throw(expected_modify)
    end,
    ok = file:close(H),
    receive
        {[close_write],0,""} -> pass;
        M4 -> throw({expected_message_file_close_write_but_got, M4})
    after 1000 -> throw(expected_close_write)
    end,
    ok = inotify:watch("/tmp/", tag3, ?ALL),
    ok = inotify:add_handler(tag3, ?MODULE, self()),
    receive after 100 -> ok end,
    ok = file:delete("/tmp/inotify_test"),
    receive
        {[attrib],0,""} -> pass
    after 1000 ->
            receive X1 -> throw({expected, X1})
            after 1000 -> throw(expected_attrib)
            end
    end,
    receive
        {[delete_self],0,""} -> pass
    after 1000 ->
            receive X2 -> throw({expected, X2})
            after 1000 -> throw(expected_delete_self)
            end
    end,
    receive
        {[ignored],0,""} -> pass
    after 1000 ->
            receive X3 -> throw({expected, X3})
            after 1000 -> throw(expected_ignored)
            end
    end,
    receive
        {[delete],0,"inotify_test"} -> pass
    after 1000 ->
            receive X4 -> throw({expected, X4})
            after 1000 -> throw(expected_delete)
            end
    end.

inotify_event(TestPid, Tag, Msg = ?inotify_msg(M, C, N)) ->
    io:format("$$$ ~p: ~p~n", [Tag, Msg]),
    TestPid ! {M, C, N},
    ok.
