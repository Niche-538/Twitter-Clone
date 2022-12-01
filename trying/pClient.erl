-module(pClient).
-export([client_fun/5]).

client_fun(ServerID, UserName,TweetsNumber,SubscribersNumber,ExistingUser)->
  if
    ExistingUser == 1 ->
      io:fwrite("User Online: ~p ~n",[UserName]),
      handle_login(UserName,ServerID);
    true ->
      io:fwrite("New User Registeration. User Name: ~p ~n",[UserName]),
      ServerID ! {registerUser, {UserName, self()}},
      receive
        {acknowledgement} -> io:fwrite("Registration Successful for User: ~p ~n",[UserName])
      end,
      handle_client(UserName,TweetsNumber,SubscribersNumber,ServerID)
  end.


handle_login(UserName,ServerID) ->
  ServerID ! {login,{UserName,self()}},
  loop(5,UserName,ServerID).

loop(N, UserName, ServerID) ->
  if
    N =< 0 -> ok;
    true ->
      UniqueHash = generate_unique_hashes(UserName, N),
      ServerID ! {tweets, {UserName, (UserName ++ "I am Tweeting some gibberish tag #" ++ UniqueHash)}}
  end,
  loop(N-1,UserName,ServerID).


generate_unique_hashes(User, N) ->
  BitcoinKey = User ++ integer_to_list(N),
  <<BKey:256>> = crypto:hash(sha256, BitcoinKey),
  _SHA_String = io_lib:format("~64.16.0b", [BKey]).

handle_client(UserName, TweetsNumber, SubscribersNumber, ServerID) ->
  io:fwrite("Client Handled").