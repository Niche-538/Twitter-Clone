-module(pETS).
-export([create_tables/0]).

%%Create Public ETS Tables
create_tables()->
  ets:new(user_table, [set, public, named_table]),
  io:fwrite("Tables Created ~n").