-module(pServer).
-export([server_fun/0]).

server_fun()->
  io:fwrite("In Server~n"),
  loop().

loop()->
  receive
    {registerUser,{UserName, PID}} ->
      ets:insert(clients_registry, {UserName, PID}),
      ets:insert(tweets, {UserName, []}),
      ets:insert(subscribed_to, {UserName, []}),
      PID ! {acknowledgement}
  end,
  loop().