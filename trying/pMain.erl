-module(pMain).
-export([main/1]).

main(NumUsers) ->
  ServerID = spawn(pServer, server_fun, [NumUsers]),
  io:fwrite("Server ID: ~p~n", [ServerID]).
