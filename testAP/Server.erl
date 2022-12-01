-module(pServer).
-export([server_fun/0]).

server_fun() ->
    io:fwrite("In Server~n"),
    loop().

loop() ->
    receive
        {registerUser, {UserName, PID}} ->
            register_user(UserName, PID),
            PID ! {acknowledgement};
        {tweets, {UserName, Tweet}} ->
            io:fwrite("Got a Tweet ~n"),
            tweet_processing(UserName, Tweet)
    end,
    loop().

register_user(UserName, PID) ->
    ets:insert(client_table, {UserName, PID}),
    ets:insert(tweet_table, {UserName, []}),
    ets:insert(subscribed_to_table, {UserName, []}),
    case ets:lookup(subscriber_table, UserName) == [] of
        true ->
            ets:insert(subscriber_table, {UserName, []});
        false ->
            ok
    end.

tweet_processing(UserName, Tweet) ->
    LookupTableResult = ets:lookup(tweet_table, UserName),
    case LookupTableResult == [] of
        true ->
            UpdatedTweet = lists:append([], [Tweet]),
            ets:insert(tweet_table, {UserName, UpdatedTweet});
        false ->
            {_, PreviousTweetList} = lists:nth(1, LookupTableResult),
            UpdatedTweet = lists:append(PreviousTweetList, [Tweet]),
            ets:insert(tweet_table, {UserName, UpdatedTweet})
    end.
