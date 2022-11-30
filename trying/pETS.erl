-module(pETS).
-export([create_tables/0]).

%%Create Public ETS Tables
create_tables()->
  ets:new(user_table, [set, public, named_table]),
  ets:new(clients_registry, [set, public, named_table]),
  ets:new(tweets, [set, public, named_table]),
  ets:new(hashtags_mentions, [set, public, named_table]),
  ets:new(subscribed_to, [set, public, named_table]),
  ets:new(followers, [set, public, named_table]),
  io:fwrite("Tables Created ~n").