-module(pServer).
-export([server_fun/1]).

server_fun(NumUsers)->
  io:fwrite("Num Users ~p~n", [NumUsers]).