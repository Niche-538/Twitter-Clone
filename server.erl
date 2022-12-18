-module(server).
-export([server_fun/0]).

server_fun() ->
    loop().

loop() ->
    receive
        {registerUser, {UserID, UserName, PID}} ->
            register_user(UserID, UserName, PID),
            PID ! {acknowledgement};
        {tweets, {UserID, Tweet, PID}} ->
            PID,
            % io:fwrite("Tweet Received from ~p: ~p~n", [UserID, Tweet]),
            tweet_processing(UserID, Tweet);
        {login, {UserID, UserName, PID}} ->
            ets:insert(client_table, {UserID, UserName, PID});
        {subscribedToTweets, {UserID, PID}} ->
            spawn(fun() -> subscribedToTweetsHandler(UserID, PID) end);
        {hashtagTweets, {Hashtag, UserID, PID}} ->
            UserID,
            spawn(fun() -> hashtagTweetsHandler(Hashtag, PID) end);
        {mentionTweets, {UserID, PID}} ->
            spawn(fun() -> mentionTweetsHandler(UserID, PID) end);
        {getAllTweets, {UserID, PID}} ->
            spawn(fun() -> getAllTweets(UserID, PID) end);
        {addFollower, {UserID, SubID, PID}} ->
            PID,
            subscribed_to_add(UserID, SubID),
            followers_to_add(UserID, SubID);
        {disconnectUser, {UserID, UserName}} ->
            disconnectUser(UserID, UserName)
    end,
    loop().

tail_len(L) ->
    tail_len(L, 0).
tail_len([], Acc) ->
    Acc;
tail_len([_ | T], Acc) ->
    tail_len(T, Acc + 1).

register_user(UserID, UserName, PID) ->
    ets:insert(client_table, {UserID, PID, UserName}),
    ets:insert(tweet_table, {UserID, []}),
    ets:insert(subscribed_to_table, {UserID, []}),
    case ets:lookup(subscriber_table, UserID) == [] of
        true ->
            ets:insert(subscriber_table, {UserID, []});
        false ->
            ok
    end.

disconnectUser(UserID, UserName) ->
    ets:insert(client_table, {UserID, nil, UserName}).

tweet_processing(UserID, Tweet) ->
    TweetLookupTableResult = ets:lookup(tweet_table, UserID),
    case TweetLookupTableResult == [] of
        true ->
            ets:insert(tweet_table, {UserID, [Tweet]});
        false ->
            {_, PreviousTweetList} = lists:nth(1, TweetLookupTableResult),
            UpdatedTweetList = lists:append(PreviousTweetList, [Tweet]),
            ets:insert(tweet_table, {UserID, UpdatedTweetList})
    end,

    % Check for hashtags and mentions just like above, and add them to the hashtag and mentions table
    {_, HX} = re:run(Tweet, "#+[a-zA-Z0-9(_)]{1,}"),
    {HIndex, HLength} = lists:nth(1, HX),
    HashtagFound = string:substr(Tweet, HIndex+1, HLength),
    HashtagLookupTableResult = ets:lookup(hashtag_table, HashtagFound),
    case HashtagLookupTableResult == [] of
        true ->
            ets:insert(hashtag_table, {HashtagFound, [Tweet, UserID]});
        false ->
            {_, PreviousHashtagTweetList} = lists:nth(1, HashtagLookupTableResult),
            UpdatedHashtagTweetList = lists:append(PreviousHashtagTweetList, [Tweet, UserID]),
            ets:insert(hashtag_table, {HashtagFound, UpdatedHashtagTweetList})
    end,

    %  Mentions
    {_, MX} = re:run(Tweet, "@+[a-zA-Z0-9(_)]{1,}"),
    {MIndex, MLength} = lists:nth(1, MX),
    MentionFound = string:substr(Tweet, MIndex + 1, MLength),

    MentionLookupTableResult = ets:lookup(mention_table, MentionFound),
    case MentionLookupTableResult == [] of
        true ->
            ets:insert(mention_table, {MentionFound, [Tweet, UserID]});
        false ->
            {_, PreviousMentionTweetList} = lists:nth(1, MentionLookupTableResult),
            UpdatedMentionTweetList = lists:append(PreviousMentionTweetList, [Tweet, UserID]),
            ets:insert(mention_table, {MentionFound, UpdatedMentionTweetList})
    end,

    % Send Tweets to Subscribers
    SubscriberLookupTable = ets:lookup(subscriber_table, UserID),
    {_, SubscribersList} = lists:nth(1, SubscriberLookupTable),
    case SubscribersList == [] of
        true ->
            done;
        false ->
            lists:foreach(
                fun(ID) ->
                    FindPID = ets:lookup(client_table, ID),
                    {_, PID, _} = lists:nth(1, FindPID),
                    PID ! {ackLive, {Tweet}}
                end,
                SubscribersList
            )
    end.

subscribedToTweetsHandler(UserID, PID) ->
    SubscribedToList = getSubscribedTo(UserID),
    case SubscribedToList == [] of
        true ->
            PID ! {ackTweetSubscription, {[]}};
        false ->
            TweetList = generateTweetList(SubscribedToList, [], tail_len(SubscribedToList)),
            io:fwrite("Subscribed To List for ~p: ~p~nTweet list for ~p: ~p~n", [UserID, SubscribedToList, PID, TweetList]),
            PID ! {ackTweetSubscription, {TweetList}}
    end.

%% get list of users that USERID is subscribed to.
getSubscribedTo(UserID) ->
    SubscribedToLookupTableResult = ets:lookup(subscribed_to_table, UserID),
    {_, SubscribedToUsersList} = lists:nth(1, SubscribedToLookupTableResult),
    SubscribedToUsersList.

%% get tweets from the subscribedTo user list
generateTweetList(SubscribedToList, Listed, Len) ->
    case Len == 0 of
        true -> Listed;
        false ->
            GT = getTweets(lists:nth(Len, SubscribedToList)),
            generateTweetList(SubscribedToList, Listed ++ GT, Len - 1)
    end.

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
    UpdatedList = lists:append(SubscribedToList, [SubID]),
    ets:insert(subscribed_to_table, {UserID, UpdatedList}).

followers_to_add(UserID, SubID) ->
    SubID,
    SubscribedToAddResult = ets:lookup(subscriber_table, SubID),
    {_, FollowersToList} = lists:nth(1, SubscribedToAddResult),
    UpdatedSubscriberList = lists:append(FollowersToList, [UserID]),
    ets:insert(subscriber_table, {SubID, UpdatedSubscriberList}).
