#==============================================================================
# 01-load-politician-friends
# Purpose: Download info of congressmen's friends (accounts they follow)
# Author: Haohan Chen
#==============================================================================

rm(list=ls())
source('utils/get-friends.R')
source('utils/oauth.R')


##############################
# Oauth
##############################

# Setup oauth
# Get oauth
oauth_keys <- read.csv("Data/Oauth_keys.csv", stringsAsFactors = F)
oauth <- list()
for (i in 1:nrow(oauth_keys)){
  oauth[[as.character(i)]] <- ROAuth::OAuthFactory$new(consumerKey=oauth_keys$consumerKey[i],
                                         consumerSecret=oauth_keys$consumerSecret[i],
                                         oauthKey=oauth_keys$oauthKey[i],
                                         oauthSecret=oauth_keys$oauthSecret[i],
                                         needsVerifier=FALSE,
                                         handshakeComplete=TRUE,
                                         verifier="1",
                                         requestURL="https://api.twitter.com/oauth/request_token",
                                         authURL="https://api.twitter.com/oauth/authorize",
                                         accessURL="https://api.twitter.com/oauth/access_token",
                                         signMethod="HMAC")
}


##############################
# Scrape the list of congressmen's friends
##############################


# Read the congressmen dataset
Congressmen <- read.csv("Data/Congressmen.csv", stringsAsFactors = F)
screen_names <- Congressmen$screen_name

if ("Congressmen_Friends_list.Rda" %in% list.files("Data")){
  load("Data/Congressmen_Friends_list.Rda")
} else{
  id_friends <- list()
}

# Collect friend's ID's
t_checkpoint <- Sys.time()
to_collect <- screen_names[!screen_names %in% names(id_friends)]
for (i in 1:length(to_collect)){
  print(to_collect[i])
  id_friends[[to_collect[i]]] <- getFriends(screen_name = to_collect[i], oauth = oauth)
  if (as.numeric(Sys.time() - t_checkpoint) > 300){
    save(id_friends, file="Data/Congressmen_Friends_list.Rda")
    t_checkpoint <- Sys.time()
  }
}
list_friends <- do.call(c, id_friends)
friends_freq <- as.data.frame(table(list_friends))
friends_freq <- subset(friends_freq, !list_friends %in% as.character(Congressmen$id_str))
friends_freq <- friends_freq[order(friends_freq$Freq, decreasing=T),]

id_friends$friends_freq <- friends_freq
save(id_friends, file="Data/Congressmen_Friends_list.Rda")



##############################
# Scrape user info of politicians' friends
##############################
source('utils/get-users-batch.R')
source('utils/oauth.R')

load("Data/Congressmen_Friends_list.Rda")
friends_freq <- id_friends$friends_freq


users.df <- getUsersBatch(ids=as.character(friends_freq$list_friends), my_oauth = oauth, include_entities="false",
                          verbose=TRUE, output=NULL, fname = "Congressmen_Friends_info.Rda")


##############################
# Re-organize user info of politicians' friends into a table
##############################

load("Data/Congressmen_Friends_info.Rda")

Friends_info <- do.call(rbind.data.frame, users.df)
Friends_info2 <- merge(Friends_info, friends_freq, by.x="id_str", by.y="list_friends")
names(Friends_info2)[which(names(Friends_info2) == "Freq")] <- "N_Friends_Congressmen"

hist(friends_freq$Freq)
length(Friends_info2$Freq[Friends_info2$Freq>5])

Congressmens_Friends_table <- Friends_info2
rm(Friends_info, Friends_info2)

save(Congressmens_Friends_table, file="Data/Congressmens_Friends_table.Rda")
