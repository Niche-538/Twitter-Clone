# Twitter-Clone
## Member Names:
  ### Anurag Patil
  ### Pratik Kamble

## Project Summary:

In this project, we have implemented Twitter Like Engine with the following features:
1.	Register account
2.	Send tweet with Hashtags and Mentions other users. 
3.	Follow other users.
4.	Retweet the tweets from users you follow
5.	Query Tweets by followers, hashtags and mentions.
6.	Allow Live Tweet Updates for online users.

### We have tested our code by
1.	Simulating for over 2500 clients.
2.	Simulating connection and disconnection of users.
3.	Simulating Zipf distribution on the number of Subscribers.

### Steps to run the code:
To run the code run following commands:
1. c(ets_records).
2. c(main).
3. c(server).
4. c(client).
5. ets_records:create_tables().
6. main:main(NumofClients, SubscriberLimit, OfflineClientPercentage).

### Architecture:
The architecture consist of following elements:
1.	Client – client.erl
2.	Twitter Engine – server.erl
3.	Database – ets_records.erl
