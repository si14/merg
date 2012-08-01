-module(merg_watcher).
-behaviour(gen_server).

-include("merg.hrl").

%% API
-export([start_link/0]).
-export([watch/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EPOCH_START, 62167219200). %% gregorian seconds of 1970.1.1 0:0:0
-define(RESCAN_DELAY, 300). %% files will be rescanned for changes with this
                            %% delay

-record(state, {doc_dir :: string()}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec watch(string(), [#app{}], boolean()) -> ok.
watch(DocDir, RawApps, ShouldWatch) ->
    gen_server:call(?SERVER, {init, DocDir, RawApps, ShouldWatch}).

%%% gen_server callbacks
init([]) ->
    %% erlang:send_after(3000, self(), please_die),
    io:format("merg_watcher started~n"),
    ets:new(merg_apps, [named_table, {keypos, #app.name}]),
    ets:new(merg_mods, [named_table, {keypos, #mod.name}]),
    {ok, #state{}}.

handle_call({init, DocDir, RawApps, ShouldWatch}, _From, State) ->
    Apps = [fetch_app(A) || A <- RawApps],
    Mods = lists:concat([fetch_mods(A) || A <- Apps]),
    ets:insert(merg_apps, Apps),
    ets:insert(merg_mods, Mods),
    AppNames = [A#app.name || A <- Apps],
    render_apps(DocDir, AppNames),
    render_mods(DocDir, Mods),
    case ShouldWatch of
        false -> ok;
        true ->
            [begin
                 Delay = trunc(?RESCAN_DELAY * (1 + random:uniform())),
                 erlang:send_after(Delay, self(), {rescan_mod, M#mod.name})
             end || M <- Mods]
    end,
    {reply, ok, State#state{doc_dir=DocDir}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rescan_app, _A}, State) ->
    {noreply, State};
handle_info({rescan_mod, ModName}=Msg, #state{doc_dir=DocDir}=State) ->
    [Mod] = ets:lookup(merg_mods, ModName),
    Lastmod = lastmod(Mod#mod.file),
    case Lastmod > Mod#mod.last_mod of
        true ->
            render_mod(DocDir, Mod),
            ets:insert(merg_mods, Mod#mod{last_mod=Lastmod});
        false ->
            ok
    end,
    erlang:send_after(?RESCAN_DELAY, self(), Msg),
    {noreply, State};
handle_info(please_die, State) ->
    io:format("merg_watcher dies~n"),
    application:stop(merg),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
%% ------------------

fetch_app(RawApp) ->
    {ok, [Spec]} = file:consult(RawApp#app.file),
    AppName = RawApp#app.name,
    {application, AppName, Details} = Spec,
    Description = proplists:get_value(description, Details, ""),
    Version = proplists:get_value(vsn, Details, "???"),
    Applications = proplists:get_value(applications, Details, []),
    RawApp#app{description=Description,
               version=Version,
               applications=Applications,
               last_mod=lastmod(RawApp#app.file)}.

fetch_mods(#app{name=AppName, dir=Dir}) ->
    ModFiles = filelib:fold_files(Dir, ".+\.erl$", true,
                                  fun(F, A) -> [F|A] end, []),
    [begin
         ModName = filename:basename(ModFile, ".erl"),
         {ok, Body} = file:read_file(ModFile),
         #mod{name=list_to_atom(ModName),
              app=AppName,
              file=ModFile,
              body=Body,
              last_mod=lastmod(ModFile)}
     end || ModFile <- ModFiles].

render_apps(DocDir, Apps) ->
    [render_app(DocDir, A) || A <- Apps],
    ok.

render_app(_DocDir, AppName) ->
    io:format("rendering app: ~p [STUB]~n", [AppName]).

render_mods(DocDir, Mods) ->
    [render_mod(DocDir, M) || M <- Mods].

render_mod(DocDir, Mod) ->
    [App] = ets:lookup(merg_apps, Mod#mod.app),
    AppDocDir = filename:join([rebar_utils:get_cwd(), DocDir,
                               atom_to_list(App#app.name)]) ++ "/",
    ok = filelib:ensure_dir(AppDocDir),
    Doc = merg_pygate:process(atom_to_binary(Mod#mod.name, utf8),
                              Mod#mod.body),
    DocFile = filename:join(AppDocDir,
                            atom_to_list(Mod#mod.name) ++ ".html"),
    ok = file:write_file(DocFile, Doc),
    io:format("wrote file ~s~n", [DocFile]).

lastmod(File) ->
    DT = filelib:last_modified(File),
    calendar:datetime_to_gregorian_seconds(DT).

%% greg_now() ->
%%     {MegaSecs, Secs, _} = erlang:now(),
%%     ?EPOCH_START + MegaSecs * 1000000 + Secs.
