-module(merg_pygate).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([process/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PYSCRIPT_PATH, "priv/merg.py").
-define(SERVER, ?MODULE).

-record(state, {port :: port()}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process(Str) ->
    gen_server:call(?SERVER, {process, Str}).

%%% gen_server callbacks
init([]) ->
    Port = open_port({spawn, "python -u " ++ ?PYSCRIPT_PATH},
                     [{packet, 4}, binary, nouse_stdio]),
    {ok, #state{port=Port}}.

handle_call({process, Str}=Msg, _From, #state{port=Port}=State) ->
    true = is_binary(Str),
    Req = term_to_binary(Msg),
    port_command(Port, Req),
    Res = receive
              {Port, {data, RawResponse}} ->
                  binary_to_term(RawResponse)
          end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
