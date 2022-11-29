-module(pMain).
-export([main/0]).

main() ->
  ServerID = spawn(pServer, server_fun, []),
  io:fwrite("Server ID: ~p~n", [ServerID]),
  NumClients = 10,
  MaxSubscribers = 10,
  ets:new(user_table, [set, named_table]),
  ets:insert(user_table, {pratik, 10,10}),
  ets:lookup(user_table, 1),
  ServerID ! {atom}.



