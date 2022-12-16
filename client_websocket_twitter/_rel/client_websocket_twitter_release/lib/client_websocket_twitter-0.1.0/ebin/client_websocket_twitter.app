{application, 'client_websocket_twitter', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['client_websocket_twitter_app','client_websocket_twitter_sup']},
	{registered, [client_websocket_twitter_sup]},
	{applications, [kernel,stdlib]},
	{mod, {client_websocket_twitter_app, []}},
	{env, []}
]}.