-module(client).
-export([client_fun/6]).

client_fun(ServerID, UserID, UserName, TweetsNumber, SubscribersNumber, ExistingUser) ->
    if
        ExistingUser == 1 ->
            io:fwrite("User Online: User Id: ~p User Name: ~p ~n", [UserID, UserName]),
            handle_login(UserID, UserName, ServerID);
        true ->
            io:fwrite("New User Registeration. User Id: ~p User Name: ~p ~n", [UserID, UserName]),
            ServerID ! {registerUser, {UserID, UserName, self()}},
            receive
                {acknowledgement} ->
                    io:fwrite("Registration Successful User Id: ~p User Name: ~p ~n", [
                        UserID, UserName
                    ])
            end,
            handle_client(UserID, UserName, TweetsNumber, SubscribersNumber, ServerID)
    end.

handle_login(UserID, Username, ServerID) ->
    ServerID ! {login, {UserID, Username, self()}},
    loop(5, UserID, Username, ServerID).

loop(N, UserID, UserName, ServerID) ->
    if
        N =< 0 ->
            ok;
        true ->
            % UniqueHash = generate_unique_hashes(UserName, N),
            ServerID !
                {tweets,
                    {UserID, (UserName ++ "I am Tweeting some gibberish tag #HelloWorld for @1"),
                        self()}}
    end,
    loop(N - 1, UserID, UserName, ServerID).

% generate_unique_hashes(User, N) ->
%     GeneratedHash = User ++ integer_to_list(N),
%     <<HashKey:256>> = crypto:hash(sha256, GeneratedHash),
%     _SHA_String = io_lib:format("~64.16.0b", [HashKey]).

handle_client(UserID, UserName, TweetsNumber, SubscribersNumber, ServerID) ->
    %%Subscribe
    case SubscribersNumber > 0 of
        true ->
            FollowerList = getFollowerList(1, SubscribersNumber, []),
            makeZipfDistribution(UserID, FollowerList, ServerID);
        false ->
            done
    end,
    %%Mention
    % MentionUser = trunc(rand:uniform(UserID)),
    ServerID !
        %%        {tweets, {UserID, "User" ++ UserName ++ "is a friend of #HelloWorld User ID @" ++ MentionUser, self()}},
        {tweets, {UserID, "User " ++ UserName ++ "is a friend of #HelloWorld User ID @1", self()}},

    %% Hash tag
    ServerID ! {tweets, {UserID, "User " ++ UserName ++ "has a hashtag #HelloWorld @1", self()}},

    %%Send Tweets
    sendTweets(TweetsNumber, UserID, UserName, ServerID),

    %%Retweet
    retweet_handler(UserID, UserName, ServerID),
    % Add Retweet Time Difference

    %%Queries
    subscription_query_handler(UserID, UserName, ServerID),
    hashtag_query_handler("#HelloWorld", UserID, UserName, ServerID),
    mention_query_handler(UserID, UserName, ServerID),
    %%Get all Tweets
    getAllTweets(UserID, UserName, ServerID),
    %%Live View
    getLiveView(UserName).

getFollowerList(N, SubscribersNumber, List) ->
    case N == SubscribersNumber of
        true -> [N | List];
        false -> getFollowerList(N + 1, SubscribersNumber, [N | List])
    end.

makeZipfDistribution(UserID, FollowerList, ServerID) ->
    lists:foreach(
        fun(N) ->
            ServerID ! {addFollower, {UserID, N, self()}}
        end,
        FollowerList
    ).

sendTweets(Counter, UserID, UserName, ServerID) ->
    case Counter > 0 of
        true ->
            ServerID !
                {tweets, {UserID, "User " ++ UserName ++ "tweets gibberish #HelloWorld @1"}};
        false ->
            done
    end,
    sendTweets(Counter - 1, UserID, UserName, ServerID).

retweet_handler(UserID, UserName, ServerID) ->
    ServerID ! {subscribedToTweets, {UserID, self()}},
    receive
        {retweet_ack, {ReTweetList}} ->
            ToRetweet = lists:nth(1, ReTweetList),
            case ToRetweet == [] of
                true ->
                    done;
                false ->
                    ServerID !
                        {tweets,
                            {UserID,
                                "User:" ++ UserName ++ "#HelloWorld @1 Retweeting: " ++ ToRetweet,
                                self()}}
            end
    end.

subscription_query_handler(UserID, UserName, ServerID) ->
    ServerID ! {subscribedToTweets, {UserID, self()}},
    receive
        {ackTweetSubscription, {ListOfTweets}} ->
            case ListOfTweets == [] of
                true ->
                    done;
                false ->
                    io:fwrite("User Name: ~p Subscribed Tweets: ~n ~p ~n~n", [
                        UserName, ListOfTweets
                    ])
            end
    end.

hashtag_query_handler(Hashtag, UserID, UserName, ServerID) ->
    ServerID ! {hashtagTweets, {Hashtag, UserID, self()}},
    receive
        {ackTweetHashtag, {ListOfTweets}} ->
            case ListOfTweets == [] of
                true ->
                    done;
                false ->
                    io:fwrite("User Name: ~p  Hashtag Tweets: ~n ~p ~n~n", [UserName, ListOfTweets])
            end
    end.

mention_query_handler(UserID, UserName, ServerID) ->
    ServerID ! {mentionTweets, {UserID, self()}},
    receive
        {ackMentionHashtag, {ListOfTweets}} ->
            case ListOfTweets == [] of
                true ->
                    done;
                false ->
                    io:fwrite("User Name: ~p  Mentioned Tweets: ~n ~p ~n~n", [
                        UserName, ListOfTweets
                    ])
            end
    end.

getAllTweets(UserID, UserName, ServerID) ->
    ServerID ! {getAllTweets, {UserID, self()}},
    receive
        {ackAllTweets, {ListOfTweets}} ->
            case ListOfTweets == [] of
                true ->
                    done;
                false ->
                    io:fwrite("User Name: ~p  All Tweets: ~n ~p ~n~n", [UserName, ListOfTweets])
            end
    end.

getLiveView(UserName) ->
    receive
        {ackLive, {Tweets}} -> io:fwrite("User Name: ~p Live View: ~n ~p ~n~n", [UserName, Tweets])
    end,
    getLiveView(UserName).
