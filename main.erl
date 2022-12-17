-module(main).
-export([main/3]).

main(NumClients, SubscriberLimit, OfflineClientsPercentage) ->
    %%Create Server Engine Node
    ServerID = spawn(server, server_fun, []),
    OfflineClients = (OfflineClientsPercentage * 0.01) * NumClients,
    createUsers(NumClients, SubscriberLimit, ServerID, NumClients),
    offlineSimulation(NumClients, OfflineClients, ServerID).

createUsers(Counter, SubscriberLimit, ServerID, NumClients) ->
    case Counter == 0 of
        true ->
            ok;
        false ->
            UserID = Counter,
            UserName = "@tweetstar" ++ integer_to_list(Counter),
            TweetsNumber = trunc(SubscriberLimit / Counter),
            SubscribersNumber = round(trunc(SubscriberLimit / (NumClients - Counter + 1))) + 1,
            PID = spawn(client, client_fun, [
                ServerID, UserID, UserName, TweetsNumber, SubscribersNumber, 0
            ]),
            ets:insert(main_table, {UserID, PID}),
            createUsers(Counter - 1, SubscriberLimit, ServerID, NumClients)
    end.

offlineSimulation(NumClients, OfflineClients, ServerID) ->
    DisconnectList = disconnection_handler(NumClients, OfflineClients, 0, [], ServerID),
    lists:foreach(
        fun(UserID) ->
            PID = spawn(client, client_fun, [ServerID, UserID, nil, nil, nil, 1]),
            ets:insert(main_table, {UserID, PID})
        end,
        DisconnectList
    ).

disconnection_handler(NumClients, OfflineClients, DisconnectedClients, DisconnectedList, ServerID) ->
    case OfflineClients < DisconnectedClients of
        true ->
            OfflineClient = rand:uniform(NumClients),
            {_, OfflineClientId} = ets:lookup(main_table, OfflineClient),
            case OfflineClientId == nil of
                true ->
                    disconnection_handler(
                        NumClients, OfflineClients, DisconnectedClients, DisconnectedList, ServerID
                    );
                false ->
                    UpdatedList = lists:append(DisconnectedList, OfflineClient),
                    ServerID ! {disconnectUser, {OfflineClient}},
                    ets:insert(main_table, {OfflineClient, nil}),
                    io:fwrite("User Id : ~p Disconnected ~n", [OfflineClient]),
                    disconnection_handler(
                        NumClients, OfflineClients, DisconnectedClients, UpdatedList, ServerID
                    )
            end;
        false ->
            DisconnectedList
    end.
