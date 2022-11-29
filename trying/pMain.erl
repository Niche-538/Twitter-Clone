-module(pMain).
-export([main/0]).

main() ->
  %%Create Server Engine Node
  ServerID = spawn(pServer, server_fun, []),
  io:fwrite("Server ID: ~p~n", [ServerID]),

  NumClients = 10,
  MaxSubscribers = 10,
  createUsers(NumClients,MaxSubscribers),
  ServerID ! {atom}.

createUsers(NumClients, MaxSubscribers) ->
  io:fwrite("NumClients ~p MaxSubcribers: ~p~n", [NumClients,MaxSubscribers]).