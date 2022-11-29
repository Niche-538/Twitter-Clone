-module(pMain).
-export([main/0]).

main() ->
  %%Create Server Engine Node
  ServerID = spawn(pServer, server_fun, []),
  io:fwrite("Server ID: ~p~n", [ServerID]),

  NumClients = 10,
  createUsers(NumClients,ServerID).

createUsers(NumClients,ServerID) ->
  if
    NumClients == 0 -> ok;
    true ->
      spawn(pClient, client_fun, [ServerID]),
      io:fwrite("Loop Num Clients: ~p~n", [NumClients]),
      createUsers(NumClients-1,ServerID)
  end.
