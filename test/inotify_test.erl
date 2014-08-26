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

    inotify:start_link(),
    Ref1 = inotify:watch("/tmp/", ?ALL),
    ok = inotify:add_handler(Ref1, ?MODULE, self()),
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
    Ref2 = inotify:watch("/tmp/inotify_test"),
    inotify:unwatch(Ref1),
    ok = inotify:add_handler(Ref2, ?MODULE, self()),
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
    Ref3 = inotify:watch("/tmp/", ?ALL),
    ok = inotify:add_handler(Ref3, ?MODULE, self()),
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
    end,
    
    exit(whereis(inotify_evt), normal),
    exit(whereis(inotify_server), normal).

inotify_event(TestPid, Ref, Msg = ?inotify_msg(M, C, N)) ->
    io:format("$$$ ~p: ~p~n", [Ref, Msg]),
    TestPid ! {M, C, N},
    ok.
