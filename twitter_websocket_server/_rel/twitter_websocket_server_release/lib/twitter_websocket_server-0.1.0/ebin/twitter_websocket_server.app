{application, 'twitter_websocket_server', [
	{description, "Twitter Like Server"},
	{vsn, "0.1.0"},
	{modules, ['tweet_handler','twitter_websocket_server_app','twitter_websocket_server_sup']},
	{registered, [twitter_websocket_server_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {twitter_websocket_server_app, []}},
	{env, []}
]}.