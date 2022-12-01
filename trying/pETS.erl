-module(pETS).
-export([create_tables/0]).

%%Create Public ETS Tables
create_tables()->
  ets:new(main_table, [set, public, named_table]),
  ets:new(client_table, [set, public, named_table]),
  ets:new(tweet_table, [set, public, named_table]),
  ets:new(hashtag_mention_table, [set, public, named_table]),
  ets:new(subscribed_to_table, [set, public, named_table]),
  ets:new(subscriber_table, [set, public, named_table]),
  io:fwrite("Tables Created ~n").