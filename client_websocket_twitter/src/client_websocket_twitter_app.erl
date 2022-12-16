-module(client_websocket_twitter_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([{'_', [{"/", client_handler, []}]}]),
	{ok, _} = cowboy:start_clear(my_http_listener, [{port, 8081}], #{env => #{dispatch => Dispatch}}),
	client_websocket_twitter_sup:start_link().

stop(_State) ->
	ok.
