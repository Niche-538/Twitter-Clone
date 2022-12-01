-module(pMain).
-export([main/3]).

main(NumClients,SubscriberLimit,OfflineClientsPercentage) ->
  %%Create Server Engine Node
  ServerID = spawn(pServer, server_fun, []),
  io:fwrite("Server ID: ~p~n", [ServerID]),
  _OfflineClients = (OfflineClientsPercentage * 0.01) * NumClients,
  createUsers(NumClients, SubscriberLimit, ServerID, NumClients).

createUsers(Counter,SubscriberLimit, ServerID, NumClients) ->
  if
    Counter == 0 -> ok;
    true ->
      UserName = "chew" ++ integer_to_list(Counter),
      TweetsNumber = trunc(SubscriberLimit/Counter),
      SubscribersNumber = trunc(SubscriberLimit/(NumClients-Counter+1))-1,
      io:fwrite("Subscriber Limit ~p~n",[SubscribersNumber]),
      PID = spawn(pClient, client_fun, [ServerID, UserName,TweetsNumber,SubscribersNumber,1]),
      ets:insert(main_table,{UserName,PID}),
      createUsers(Counter-1, SubscriberLimit, ServerID, NumClients)
  end.
