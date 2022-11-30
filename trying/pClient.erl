-module(pClient).
-export([client_fun/5]).

client_fun(ServerID, UserName,TweetsNumber,SubscribersNumber,ExistingUser)->
  if
    ExistingUser == 1 ->
      io:fwrite("User Online: ~p ~n",[UserName]),
      handle_login(UserName,ServerID);
    true -> io:fwrite("New User Registeration. User Name: ~p ~n",[UserName]),
            ServerID ! {registerUser, {UserName, self()}},
            receive
              {acknowledgement} -> io:fwrite("Registration Successful")
            end
  end.


handle_login(UserName,ServerID) ->
  ServerID ! {login,{UserName,self()}},
  loop(5,UserName,ServerID).

loop(N, UserName, ServerID) ->
  if
    N =< 0 -> ok;
    true ->
      UniqueHash = generate_unique_hashes(UserName, N),
      io:fwrite("Unique Hash Test for user ~p and N ~p: ~p ~n", [UserName, N, UniqueHash]),
      ServerID ! {tweet, {UserName ++ "I am Tweeting some gibberish tag #" ++ UniqueHash}}
  end,
  loop(N-1,UserName,ServerID).


generate_unique_hashes(User, N) ->
  BitcoinKey = User ++ integer_to_list(N),
  <<BKey:256>> = crypto:hash(sha256, BitcoinKey),
  SHA_String = io_lib:format("~64.16.0b", [BKey]).