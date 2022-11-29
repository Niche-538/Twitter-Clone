-module(pServer).
-export([server_fun/0]).

server_fun()->
  io:fwrite("In Server~n"),
  loop().

loop()->
  receive
    {atom} -> io:fwrite("Listening~n"),
      io:fwrite("~p~n",[ets:lookup(user_table, 1)]),
      io:fwrite("~p~n",[ets:lookup(user_table, 2)]),
      io:fwrite("End~n")
  end,
  loop().