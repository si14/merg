-module(merg).
-behaviour(application).
-compile(export_all).

-include("merg.hrl").
%% application callbacks
-export([start/2, stop/1]).

%% This code is borrowed from Tim Watson's (watson.timothy@gmail.com)
%% rebar_skip_deps plugin
%% preprocess(Config, _AppFile) ->
%%     Deps = rebar_config:get_local(Config, deps, []),
%%     [ skip_dir(code:lib_dir(App)) || {App, _Vsn} <- Deps ],

%%     %% now for the local deps
%%     DepsDir = rebar_config:get_global(deps_dir, "deps"),
%%     Cwd = rebar_utils:get_cwd(),
%%     case file:list_dir(DepsDir) of
%%         {ok, Files} ->
%%             [ skip_dir(filename:join([Cwd, DepsDir, F])) ||
%%                 F <- Files ];
%%         _ ->
%%             ok
%%     end,

%%     %% skip sub_dirs
%%     SubDirs = rebar_config:get_local(Config, sub_dirs, []),
%%     [ skip_dir(filename:join([Cwd, SubDir]))
%%       || SubDir <- SubDirs ],
%%     {ok, []}.

%% skip_dir({error, _}) ->
%%     ok;
%% skip_dir(F) ->
%%     rebar_core:skip_dir(F).

%% End of borrowed code

preprocess(Config, AppFile) ->
    %% dirty hack
    {ok, DepDirs} = rebar_deps:preprocess(Config, AppFile),
    [rebar_core:skip_dir(DepDir) || DepDir <- DepDirs],
    {ok, []}.

merg(_Config, AppFile) ->
    true = rebar_app_utils:is_app_src(AppFile),

    AppDir = filename:dirname(AppFile),
    Files = filelib:fold_files(AppDir, ".+\.erl$", true,
                               fun(F, A) -> [F|A] end, []),
    Mods = [begin
                ModName = filename:basename(File, ".erl"),
                {ok, Body} = file:read_file(File),
                #mod{name=list_to_atom(ModName),
                     path=File,
                     body=Body,
                     last_mod=filelib:last_modified(File)}
            end || File <- Files],

    Apps = rebar_config:get_global(merg_apps, []),
    NewApps = [#app{name=rebar_app_utils:app_name(AppFile),
                    app_src=AppFile,
                    mods=Mods} | Apps],
    rebar_config:set_global(merg_apps, NewApps).

post_merg(Config, AppFile) ->
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

    MergConf = rebar_config:get_local(Config, merg, []),
    DocDir = proplists:get_value(doc_dir, MergConf, "doc_merg"),
    true = is_list(DocDir),
    ShouldWatch = proplists:get_value(watch, MergConf, true),
    true = is_boolean(ShouldWatch),

    {ok, Cwd} = file:get_cwd(),

    application:load({application, sasl, [{env, [{errlog_type, error}]}]}),
    application:start(sasl),
    true = code:add_patha("ebin"),
    ok = application:start(merg),

    Apps = rebar_config:get_global(merg_apps, []),
    [begin
         AppDocDir = filename:join([Cwd, DocDir,
                                    atom_to_list(App#app.name)]) ++ "/",
         io:format("~p~n", [AppDocDir]),
         ok = filelib:ensure_dir(AppDocDir),
         [begin
              Doc = merg_pygate:process(atom_to_binary(Mod#mod.name, utf8),
                                        Mod#mod.body),
              DocFile = filename:join(AppDocDir,
                                      atom_to_list(Mod#mod.name) ++ ".html"),
              ok = file:write_file(DocFile, Doc),
              io:format("wrote file ~s~n", [DocFile])
          end || Mod <- App#app.mods]
     end || App <- Apps],

    case ShouldWatch of
        true -> merg_watch();
        false -> ok
    end,
    ok.

merg_watch() ->
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
