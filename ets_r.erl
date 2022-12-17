-module(ets_r).
-export([create_tables/0]).

%%Create Public ETS Tables
create_tables() ->
    ets:new(main_table, [set, public, named_table]),
    ets:new(client_table, [set, public, named_table]),
    ets:new(tweet_table, [set, public, named_table]),
    ets:new(hashtag_table, [set, public, named_table]),
    ets:new(mention_table, [set, public, named_table]),
    ets:new(subscribed_to_table, [set, public, named_table]),  % a user and a list of accounts to whom they have subscribed.
    ets:new(subscriber_table, [set, public, named_table]), % a user and a list of accounts that have subscribed to them.
    io:fwrite("Tables Created ~n").
