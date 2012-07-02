-module(merg_pygate).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([process/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PYSCRIPT_PATH, "priv/merg.py").
-define(SERVER, ?MODULE).

-record(state, {port :: port()}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process(Filename, Code) ->
    gen_server:call(?SERVER, {process, Filename, Code}).

%%% gen_server callbacks
init([]) ->
    Port = open_port({spawn, "python -u " ++ ?PYSCRIPT_PATH},
                     [{packet, 4}, binary, nouse_stdio]),
    self() ! init,
    {ok, #state{port=Port}}.

handle_call({process, Filename, Code}=Msg, _From, #state{port=Port}=State) ->
    true = is_binary(Filename),
    true = is_binary(Code),
    Req = term_to_binary(Msg),
    port_command(Port, Req),
    Res = receive_resp(Port),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, #state{port=Port}=State) ->
    TemplatePath = list_to_binary(filename:absname("priv/template.mako")),
    InitReq = term_to_binary({init, TemplatePath}),
    port_command(Port, InitReq),
    io:format("FOO: ~p", [receive_resp(Port)]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
receive_resp(Port) ->
    receive
        {Port, {data, RawResponse}} ->
            binary_to_term(RawResponse)
    end.
