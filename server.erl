-module(server).
-export([server_fun/0]).

server_fun() ->
    loop().

loop() ->
    receive
        {registerUser, {UserID, UserName, PID}} ->
            register_user(UserID, UserName, PID),
            PID ! {acknowledgement};

        {tweets, {UserName, Tweet, PID}} ->
            tweet_processing(UserName, Tweet);

        {login, {UserID, UserName, PID}} ->
            ets:insert(client_table, UserID, UserName);

        {subscribedToTweets, {UserID, PID}} -> spawn(fun() -> subscribedToTweetsHandler(UserID, PID) end);

        {hashtagTweets, {Hashtag, UserID, PID}} -> spawn(fun() -> hashtagTweetsHandler(Hashtag, PID) end);

        {mentionTweets, {UserID, PID}} -> spawn(fun() -> mentionTweetsHandler(UserID, PID) end);

        {getAllTweets, {UserID, PID}} -> spawn(fun() -> getAllTweets(UserID, PID) end);

        {addFollower,{UserID,SubID,PID}} ->
            subscribed_to_add(UserID,SubID),
            followers_to_add(UserID,SubID)
    end,
    loop().

register_user(UserID, UserName, PID) ->
    ets:insert(client_table, {UserID, UserName, PID}),
    ets:insert(tweet_table, {UserID, []}),
    ets:insert(subscribed_to_table, {UserID, []}),
    case ets:lookup(subscriber_table, UserID) == [] of
        true ->
            ets:insert(subscriber_table, {UserID, []});
        false ->
            ok
    end.

tweet_processing(UserID, Tweet) ->
    TweetLookupTableResult = ets:lookup(tweet_table, UserID),
    case TweetLookupTableResult == [] of
        true ->
            UpdatedTweet = lists:append([], [Tweet]),
            ets:insert(tweet_table, {UserID, UpdatedTweet});
        false ->
            {_, PreviousTweetList} = lists:nth(1, TweetLookupTableResult),
            UpdatedTweet = lists:append(PreviousTweetList, [Tweet]),
            ets:insert(tweet_table, {UserID, UpdatedTweet})
    end.
% Check for hashtags and mentions just like above, and add them to the hashtag and mentions table


subscribedToTweetsHandler(UserID, PID) ->
    SubscribedTo = getSubscribedTo(UserID),
    List = generateTweetList(SubscribedTo,[]),
    PID ! {ackTweetSubscription, {List}}.

%% get list of users that USERID is subscribed to.
getSubscribedTo(UserID) ->
    SubscribedToLookupTableResult = ets:lookup(subscribed_to_table, UserID),
    {_, SubscribedToTweetList} = lists:nth(1, SubscribedToLookupTableResult),
    SubscribedToTweetList.

%% get tweets from the subscribedTpo user list
generateTweetList(SubscribedTo, List) ->
    lists:foreach(fun(N) -> 
                  lists:append(List, getTweets(N))
                  end, SubscribedTo),
    List.

%% get list of tweets a user is subscribed to
getTweets(UserID) ->
    TweetTableResult = ets:lookup(tweet_table, UserID),
    {_, SubscribedToTweetList} = lists:nth(1, TweetTableResult),
    SubscribedToTweetList.

hashtagTweetsHandler(Hashtag, PID) ->
    HashtagTableResult = ets:lookup(hashtag_table, Hashtag),
    {_, HashtagTweetList} = lists:nth(1, HashtagTableResult),
    PID ! {ackTweetHashtag, HashtagTweetList}.

mentionTweetsHandler(UserID, PID) ->
    MentionTableResult = ets:lookup(mention_table, UserID),
    {_, MentionTweetList} = lists:nth(1, MentionTableResult),
    PID ! {ackMentionHashtag, MentionTweetList}.

getAllTweets(UserID, PID) ->
    TweetsTableResult = ets:lookup(tweet_table, UserID),
    {_, TweetList} = lists:nth(1, TweetsTableResult),
    PID ! {ackAllTweets, TweetList}.

subscribed_to_add(UserID, SubID) ->
    SubscribedToResult = ets:lookup(subscribed_to_table, UserID),
    {_, SubscribedToList} = lists:nth(1, SubscribedToResult),
    UpdatedList = lists:append(SubscribedToList, SubID),
    ets:insert(subscribed_to_table, {UserID, UpdatedList}).

followers_to_add(UserID, SubID) ->
    FollowersToAddResult = ets:lookup(subscriber_table, UserID),
    {_, FollowersToList} = lists:nth(1, FollowersToAddResult),
    case FollowersToList == [] of
          true -> ets:insert(subscriber_table, []);
          false -> ok
    end,
    FollowerResult = ets:lookup(subscriber_table, UserID),
    {_, FollowerList} = lists:nth(1, FollowerResult),
    UpdatedList = lists:append(FollowerList, SubID),
    ets:insert(subscriber_table, {UserID, UpdatedList}).
