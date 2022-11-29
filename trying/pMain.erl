-module(pMain).
-export([main/0]).

main() ->
  ServerID = spawn(pServer, server_fun, []),
  io:fwrite("Server ID: ~p~n", [ServerID]).



