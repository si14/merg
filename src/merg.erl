-module(merg).
-behaviour(application).
-compile(export_all).

%% application callbacks
-export([start/2, stop/1]).

%% This code is borrowed from Tim Watson's (watson.timothy@gmail.com)
%% rebar_skip_deps plugin
preprocess(Config, _AppFile) ->
    Deps = rebar_config:get_local(Config, deps, []),
    [ skip_dir(code:lib_dir(App)) || {App, _Vsn} <- Deps ],

    %% now for the local deps
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    Cwd = rebar_utils:get_cwd(),
    case file:list_dir(DepsDir) of
        {ok, Files} ->
            [ skip_dir(filename:join([Cwd, DepsDir, F])) ||
                F <- Files ];
        _ ->
            ok
    end,

    %% skip sub_dirs
    SubDirs = rebar_config:get_local(Config, sub_dirs, []),
    [ skip_dir(filename:join([Cwd, SubDir]))
      || SubDir <- SubDirs ],
    {ok, []}.

skip_dir({error, _}) ->
    ok;
skip_dir(F) ->
    rebar_core:skip_dir(F).

%% End of borrowed code

merg(Config, _AppFile) ->
    true = is_base_dir(), %% this should be true because of skip_dir above
    io:format("test: ~p ~p~n",
              [is_base_dir(),
               rebar_config:get_local(Config, merg, [])]),
    application:load({application, sasl, [{env, [{errlog_type, error}]}]}),
    application:start(sasl),
    true = code:add_patha("ebin"),
    Res = application:start(merg),
    RootSupRef = erlang:monitor(process, whereis(merg_sup)),
    io:format("~p~n~p~n", [Res, application:which_applications()]),
    io:format("hello world~n"),
    Rep = merg_pygate:process(<<"
%%- FOOBAR
%%- ======
-export([test/0]).

%%- hello *world*
%%- let's parse some code!

foo() -> bar.
">>),
    io:format("got reply: ~p~n", [Rep]),
    io:format("waiting for application stop~n"),
    receive
        {'DOWN', RootSupRef, _, _, _}=Msg ->
            io:format("Got ~p~n", [Msg])
    end,
    io:format("application stopped"),
    ok.

is_base_dir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).

post_clean(_Config, _AppFile) ->
    case is_base_dir() of
        true ->
            io:format("I AM POST CLEAN");
        false ->
            ok
    end,
    ok.

start(_StartType, _StartArgs) ->
    merg_sup:start_link().

stop(_State) ->
    ok.
