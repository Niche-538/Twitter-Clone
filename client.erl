-module(client).
-export([client_fun/6]).

client_fun(ServerID, UserID, UserName, TweetsNumber, SubscribersNumber, ExistingUser) ->
    case ExistingUser == 1 of
        true ->
            io:fwrite("User Online: User Id: ~p User Name: ~p ~n", [UserID, UserName]),
            handle_login(UserID, UserName, ServerID);
        false ->
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
    case N == 0 of
        true ->
            ok;
        false ->
            ServerID !
                {tweets, {UserID, UserName ++ "I am Tweeting some tag " ++ return_hash() ++" for @1", self()}},
                loop(N - 1, UserID, UserName, ServerID)
    end.

return_hash() ->
    AvailableHashtags = ["#HelloWorld", "#HalaMadrid", "#DOSP", "#UFL"],
    RandomNumber = rand:uniform(4),
    SelectedHash = lists:nth(RandomNumber, AvailableHashtags),
    SelectedHash.

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
    % ServerID !
        %%        {tweets, {UserID, "User" ++ UserName ++ "is a friend of #HelloWorld User ID @" ++ MentionUser, self()}},
        % {tweets, {UserID, "User " ++ UserName ++ " is a friend of User ID @1 with tag " ++ return_hash(), self()}},

    %% Hash tag
    % ServerID ! {tweets, {UserID, "User " ++ UserName ++ " has a hashtag @1 of " ++ return_hash(), self()}},

    %%Send Tweets
    sendTweets(TweetsNumber, UserID, UserName, ServerID, self()),
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

sendTweets(Counter, UserID, UserName, ServerID, PID) ->
    case Counter > 0 of
        true ->
            ServerID !
                {tweets, {UserID, "I am tweeting about @1 with hashtag " ++ return_hash(), PID}},
                sendTweets(Counter - 1, UserID, UserName, ServerID, PID);
        false ->
            done
    end.

retweet_handler(UserID, UserName, ServerID) ->
    UserName,
    ServerID ! {subscribedToTweets, {UserID, self()}},
    receive
        {retweet_ack, {ReTweetList}} ->
            ToRetweet = lists:nth(1, ReTweetList),
            case ToRetweet == [] of
                true ->
                    done;
                false ->
                    ServerID !
                        {tweets, {UserID, "I use the hash " ++ return_hash() ++ " @1 Retweeting: " ++ ToRetweet, self()}}
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
