-module(pMain).
-export([main/0]).

main() ->
  %%Create Server Engine Node
  ServerID = spawn(pServer, server_fun, []),
  io:fwrite("Server ID: ~p~n", [ServerID]),

  NumClients = 10,
  TotalSubscriber = 5,
  createUsers(NumClients,TotalSubscriber, ServerID, NumClients).

createUsers(NumClients,TotalSubscriber, ServerID, TotalClients) ->
  if
    NumClients == 0 -> ok;
    true ->
      UserName = "chew" ++ integer_to_list(NumClients),
      TweetsNumber = trunc(TotalSubscriber/NumClients),
      SubscribersNumber = TotalSubscriber,
      spawn(pClient, client_fun, [ServerID, UserName,TweetsNumber,SubscribersNumber,0]),
      createUsers(NumClients-1,TotalSubscriber, ServerID, TotalClients)
  end.
