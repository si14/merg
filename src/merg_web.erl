-module(merg_web).

-include("merg.hrl").

-export([start/1]).

start(C) ->
    Dispatch =
        [{'_', [{[<<"static">>, '...'],
                 cowboy_http_static,
                 [{directory, C#merg.doc_dir},
                  {mimetypes, [{<<".css">>, [<<"text/css">>]},
                               {<<".js">>, [<<"application/javascript">>]},
                               {<<".html">>, [<<"text/html">>]}]}]},
                {[<<"watch">>],
                 merg_web_watcher, []}]}],

    case {C#merg.watch, C#merg.serve} of
        {true, true} ->
            {ok, _} =
                cowboy:start_listener(
                  merg_listener, 3,
                  cowboy_tcp_transport, [{ip, {127,0,0,1}},
                                         {port, C#merg.serve_port}],
                  cowboy_http_protocol, [{dispatch, Dispatch}]),
                ok;
        _ -> ok
    end.
