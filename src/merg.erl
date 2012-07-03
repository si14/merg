-module(merg).
-behaviour(application).
-compile(export_all).

-include("merg.hrl").
%% application callbacks
-export([start/2, stop/1]).

merg(Config, AppFile) ->
    case is_base_dir() of
        true -> merg_core(Config, AppFile);
        false -> ok
    end.

post_clean(Config, _AppFile) ->
    case is_base_dir() of
        true ->
            MergConf = rebar_config:get_local(Config, merg, []),
            DocDir = proplists:get_value(doc_dir, MergConf, "doc_merg"),
            true = is_list(DocDir),

            rebar_file_utils:rm_rf(DocDir),
            ok;
        false ->
            ok
    end,
    ok.

merg_core(Config, _AppFile) ->
    true = is_base_dir(), %% this should be true because of skip_dir above

    SubDirs = rebar_config:get_local(Config, sub_dirs, []),

    RelAppFiles = lists:append(
                    [filelib:fold_files(Dir, ".+\.app\.src$", true,
                                        fun(F, A) -> [F|A] end, [])
                     || Dir <- ["src" | SubDirs]]),
    RawApps = [begin
                   AppFile = filename:absname(RelAppFile),
                   Parts = filename:split(AppFile),
                   AppName = list_to_atom(
                               filename:rootname(lists:last(Parts),
                                                 ".app.src")),
                   AppDir = filename:join(
                              lists:sublist(Parts, length(Parts) - 1)),
                   #app{name=AppName,
                        dir=AppDir,
                        file=AppFile}
               end || RelAppFile <- RelAppFiles],

    MergConf = rebar_config:get_local(Config, merg, []),
    DocDir = proplists:get_value(doc_dir, MergConf, "doc_merg"),
    true = is_list(DocDir),
    ShouldWatch = proplists:get_value(watch, MergConf, true),
    true = is_boolean(ShouldWatch),

    application:start(sasl),
    true = code:add_patha("ebin"), %% FIXME(Dmitry): eliminate this
    true = code:add_patha("deps/inotify/ebin"),
    ok = application:start(inotify),
    ok = application:start(merg),
    merg_watcher:watch(DocDir, RawApps, ShouldWatch),

    case ShouldWatch of
        true -> merg_wait();
        false -> ok
    end,
    ok.

merg_wait() ->
    io:format("waiting for application stop~n"),
    RootSupRef = erlang:monitor(process, whereis(merg_sup)),
    receive
        {'DOWN', RootSupRef, _, _, _}=Msg ->
            io:format("Got ~p~n", [Msg])
    end,
    io:format("application stopped").

is_base_dir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).

start(_StartType, _StartArgs) ->
    merg_sup:start_link().

stop(_State) ->
    ok.
