-module(pClient).
-export([client_fun/5]).

client_fun(ServerID, UserName,TweetsNumber,SubscribersNumber,ExistingUser)->
  if
    ExistingUser == 1 ->
      io:fwrite("~p User Online. Tweet Number: ~p, Suncribers: ~p ~n",[UserName,TweetsNumber,SubscribersNumber]);
    true -> io:fwrite("New User Registeration. User Name: ~p ~n",[UserName]),
            ServerID ! {registerUser, {UserName, self()}},
            receive
              {acknowledgement} -> io:fwrite("Registration Successful")
            end
  end.
