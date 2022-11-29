-module(pServer).
-export([server_fun/0]).

server_fun()->
  io:fwrite("In Server~n"),
%%  ets:new(user_table, [set, public, named_table]),
  loop().

loop()->
  receive
    {atom} -> io:fwrite("Listening~n"),
%%      ets:insert(user_table, {1, pratik}),
      ets:lookup(user_table, pratik),
      io:fwrite("End~n")
  end,
  loop().