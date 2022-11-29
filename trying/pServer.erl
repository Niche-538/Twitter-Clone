-module(pServer).
-export([server_fun/0]).

server_fun()->
  io:fwrite("In Server~n"),
  loop(),
  server_fun().

loop()->
  receive
    {atom} -> io:fwrite("Listening~n")
  end,
  loop().