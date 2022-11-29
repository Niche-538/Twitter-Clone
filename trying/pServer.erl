-module(pServer).
-export([server_fun/0]).

server_fun()->
  io:fwrite("In Server~n"),
  loop().

loop()->
  receive
    {registerUser} ->
      io:fwrite("Registering~n"),
      io:fwrite("Registration Successful~n")
  end,
  loop().