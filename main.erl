-module(main).
-export([main/3]).

main(NumClients, SubscriberLimit, OfflineClientsPercentage) ->
    %%Create Server Engine Node
    ServerID = spawn(server, server_fun, []),
    io:fwrite("Server ID: ~p~n", [ServerID]),
    _OfflineClients = (OfflineClientsPercentage * 0.01) * NumClients,
    createUsers(NumClients, SubscriberLimit, ServerID, NumClients).

createUsers(Counter, SubscriberLimit, ServerID, NumClients) ->
    if
        Counter == 0 ->
            ok;
        true ->
            UserID = Counter,
            UserName = "chew" ++ integer_to_list(Counter),
            TweetsNumber = trunc(SubscriberLimit / Counter),
            SubscribersNumber = round(trunc(SubscriberLimit / (NumClients - Counter + 1))),
            PID = spawn(client, client_fun, [
                ServerID, UserID, UserName, TweetsNumber, SubscribersNumber, 0
            ]),
            ets:insert(main_table, {UserID, PID}),
            createUsers(Counter - 1, SubscriberLimit, ServerID, NumClients)
    end.
