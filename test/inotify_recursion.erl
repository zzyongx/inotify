-module(inotify_recursion).

-export([inotify_event/3]).

-include_lib("eunit/include/eunit.hrl").
-include("inotify.hrl").

run_test_() ->
    {timeout, 600, [fun run_test_impl/0]}.
                           
run_test_impl() ->
    inotify:start_link(),
    Dir = "/tmp/inotify_recursion_test",
    os:cmd("rm -rf " ++ Dir),
    file:make_dir(Dir),

    watch_dir(self(), Dir),
    timer:sleep(1000),
    
    file:make_dir(filename:join([Dir, "x"])),
    {_, Dir, "x"} = wait_result(),

    file:write_file(filename:join([Dir, "x", "file"]), "x/file"),
    Dir_X = filename:join([Dir, "x"]),
    ?assertMatch({_, Dir_X, "file"}, wait_result()),
    
    file:make_dir(filename:join([Dir, "x", "xx"])),
    ?assertMatch({_, Dir_X, "xx"}, wait_result()),

    file:write_file(filename:join([Dir, "x", "xx", "file"]), "x/xx/file"),
    Dir_X_XX = filename:join([Dir, "x", "xx"]),
    ?assertMatch({_, Dir_X_XX, "file"}, wait_result()),

    file:delete(filename:join([Dir, "x", "xx", "file"])),
    ?assertMatch({_, Dir_X_XX, "file"}, wait_result()),

    file:del_dir(filename:join([Dir, "x", "xx"])),
    ?assertMatch({_, Dir_X, "xx"}, wait_result()),
    ?assertMatch({_, Dir_X_XX, ""}, wait_result()),

    file:delete(filename:join([Dir, "x", "file"])),
    ?assertMatch({_, Dir_X, "file"}, wait_result()),

    file:del_dir(filename:join([Dir, "x"])),
    ?assertMatch({_, Dir, "x"}, wait_result()),
    ?assertMatch({_, Dir_X, ""}, wait_result()),

    ?debugMsg("inotify end"),

    exit(whereis(inotify_evt), normal),
    exit(whereis(inotify_server), normal).

%% in handle_event call add_handler will lead to dead lock
watch(Pid, Dir) ->
    Ref = make_ref(),
    inotify_server:watch(Dir, Ref, [create, delete, delete_self]),
    spawn(fun() ->
                  inotify_evt:add_handler(Ref, ?MODULE, {Pid, Dir})
          end),
    ?debugFmt("add ~p", [Dir]).

unwatch(Ref, Dir) ->
    spawn(fun() ->
                  inotify_server:unwatch(Ref)
          end),
    ?debugFmt("remove ~p ~p", [Ref, Dir]).

watch_dir(Pid, Dir) ->
    watch(Pid, Dir),
    {ok, Dirs} = file:list_dir(Dir),
    lists:foreach(
      fun(".") -> skip;
         ("..") -> skip;
         (Ent) ->
              FullName = filename:join([Dir, Ent]),
              case filelib:is_dir(FullName) of
                  true -> watch_dir(Pid, FullName);
                  _ -> skip
              end
      end, Dirs).

inotify_event({Pid, Dir}, Ref, ?inotify_msg(Masks, Cookie, OptionalName)) ->
    ?debugFmt("~p ~p ~p ~p ~p~n", [Ref, Dir, Masks, Cookie, OptionalName]),
    case {lists:member(create, Masks), lists:member(delete_self, Masks)} of
        {false, false} -> skip;
        {Create, Delete} ->
            FullName = filename:join([Dir, OptionalName]),
            case lists:member(isdir, Masks) of
                true when Create == true -> ?debugMsg("create"), watch_dir(Pid, FullName);
                _    when Delete == true -> ?debugMsg("delete"), unwatch(Ref, FullName);
                _ -> ?debugMsg("skip"), skip
            end
    end,
    case lists:member(ignored, Masks) of
        true -> ?debugMsg("skip send"), skip;
        _ ->
            Pid ! {Ref, Dir, OptionalName}
    end,
    ok.

wait_result() ->
    receive
        {Ref, Dir, Opt} = R -> ?debugFmt("receive ~p ~p ~p~n", [Ref, Dir, Opt]), R
    after 1000 -> ?debugMsg("recv timeout"), timeout
    end.
