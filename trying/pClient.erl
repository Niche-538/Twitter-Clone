-module(pClient).
-export([client_fun/1]).

client_fun(ServerID)->
  io:fwrite("In Client Server ID: ~p~n",[ServerID]),
  ServerID ! {registerUser}.