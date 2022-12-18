-module(client).
-export([client_fun/6]).

tail_len(L) ->
    tail_len(L, 0).
tail_len([], Acc) ->
    Acc;
tail_len([_ | T], Acc) ->
    tail_len(T, Acc + 1).

getListOfUsers(N, UL) ->
    getListOfUsers(N, [], UL).
getListOfUsers(0, L, _) ->
    lists:reverse(L);
getListOfUsers(N, L, UL) ->
    U1 = lists:nth(N, UL),
    {_, _, User} = lists:nth(1, U1),
    getListOfUsers(N - 1, [User | L], UL).

client_fun(ServerID, UserID, UserName, TweetsNumber, SubscribersNumber, ExistingUser) ->
    case ExistingUser == 1 of
        true ->
            % io:fwrite("User Online: User Id: ~p User Name: ~p ~n", [UserID, UserName]),
            handle_login(UserID, UserName, ServerID);
        false ->
            % io:fwrite("New User Registeration. User Id: ~p User Name: ~p ~n", [UserID, UserName]),
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
                {tweets, {UserID, "I am Tweeting some tag " ++ return_hash() ++" for " ++ return_user(), self()}},
                loop(N - 1, UserID, UserName, ServerID)
    end.

return_hash() ->
    AvailableHashtags = ["#HelloWorld", "#HalaMadrid", "#DOSP", "#UFL"],
    RandomNumber = rand:uniform(4),
    SelectedHash = lists:nth(RandomNumber, AvailableHashtags),
    SelectedHash.

get_users_list() ->
    U = ets:match(client_table, '$1'),
    L = getListOfUsers(tail_len(U), U),
    L.

return_user() ->
    LU = get_users_list(),
    RandomNumber = rand:uniform(tail_len(LU)),
    SelectedUser = lists:nth(RandomNumber, LU),
    SelectedUser.

handle_client(UserID, UserName, TweetsNumber, SubscribersNumber, ServerID) ->
    %%Subscribe
    case SubscribersNumber > 0 of
        true ->
            FollowerList = getFollowerList(1, SubscribersNumber, []),
            makeZipfDistribution(UserID, FollowerList, ServerID);
        false ->
            done
    end,

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
                {tweets, {UserID, "I am tweeting about " ++ return_user() ++ " with hashtag " ++ return_hash(), PID}},
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
                        {tweets, {UserID, "I use the hash " ++ return_hash() ++ " for " ++ return_user() ++ " Retweeting: " ++ ToRetweet, self()}}
            end
    end.

subscription_query_handler(UserID, UserName, ServerID) ->
    ServerID ! {subscribedToTweets, {UserID, self()}},
    receive
        {ackTweetSubscription, {TweetList}} ->
            io:fwrite("Ack Tweet Susbcription for ~p: ~p~n", [UserID, TweetList]),
            case TweetList == [] of
                true ->
                    done;
                false ->
                    io:fwrite("User Name: ~p Subscribed Tweets: ~n ~p ~n~n", [UserName, TweetList])
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
