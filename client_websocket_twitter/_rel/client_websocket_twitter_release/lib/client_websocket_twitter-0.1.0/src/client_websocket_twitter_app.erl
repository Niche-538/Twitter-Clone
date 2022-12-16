-module(client_websocket_twitter_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	client_websocket_twitter_sup:start_link().

stop(_State) ->
	ok.
