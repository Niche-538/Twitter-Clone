{application, 'client_websocket_twitter', [
	{description, "Client Twitter Like DOSP"},
	{vsn, "0.1.0"},
	{modules, ['client_handler','client_websocket_twitter_app','client_websocket_twitter_sup']},
	{registered, [client_websocket_twitter_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {client_websocket_twitter_app, []}},
	{env, []}
]}.