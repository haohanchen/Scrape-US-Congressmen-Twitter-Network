#==============================================================================
# 03-get-politician-friends'-followers
# Purpose: Download followers' of congressmen's friend
# Author: Haohan Chen
#==============================================================================

cat("Loading Oauth, user info...")
##############################
# Oauth
##############################

# Setup oauth
# Get oauth
oauth_keys <- read.table("Data/oauth_keys.csv", stringsAsFactors = F, header = T)
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

source("utils/oauth.R")
source("utils/get-followers.R")
########################################################################################
# Create a table to monitor progress (to restore work with no loss after interruption)
########################################################################################

load(file="Data/Congressmens_Friends_table.Rda")
Congressmen_Friends_Info <- Congressmen_Friends_Info[order(Congressmen_Friends_Info$N_Friends_Congressmen,decreasing=T), ]
Congressmen_Friends_Info <- Congressmen_Friends_Info[2:nrow(Congressmen_Friends_Info), ]

hist(Congressmen_Friends_Info$N_Friends_Congressmen[1:2443], breaks = 20, 
     main = "Number of Congressmen Friends\nTop 2443 Accounts", xlab = "Number of Congressmen Friends")
summary(Congressmen_Friends_Info$N_Friends_Congressmen[1:2443])

# Get a list of IDs, along with progress checker (cursors), in case of interruption.
if (!"_log_Scrape_Followers.Rda" %in% list.files("Data/Followers_lists")){
  message("Log file not found. Create a new log.")
  IDs <- as.character(Congressmen_Friends_Info$id_str)
  Scrape_Followers_Progress <- data.frame(ID = IDs, next_cursor = -1)
  save(Scrape_Followers_Progress, file="Data/Followers_lists/_log_Scrape_Followers.Rda")
} else{
  cat("Loading log file...")
  load("Data/Followers_lists/_log_Scrape_Followers.Rda")
}

# first check if there's any list of followers already downloaded to 'outfolder'
accounts.left <- subset(Scrape_Followers_Progress, next_cursor != 0)

cat("done.\n")

# loop over the rest of accounts, downloading follower lists from API
for (i in 1:nrow(accounts.left)){
  # sample randomly one account to get followers
  current_user_id <- as.character(accounts.left$ID[i])
  current_user_cursor <- accounts.left$next_cursor[i]
  current_user_n_followers <- Congressmen_Friends_Info$followers_count[
    which(Congressmen_Friends_Info$id_str == current_user_id)]
  current_user_screenname <- Congressmen_Friends_Info$screen_name[
    which(Congressmen_Friends_Info$id_str == current_user_id)]
  cat(current_user_screenname, " cursor: ", current_user_cursor, "---", current_user_n_followers, 
      " followers --- ", (nrow(accounts.left) - i), " accounts left!\n")    
  
  # download followers (with some exception handling...) 
  getFollowers(user_id=current_user_id, screen_name=current_user_screenname, cursor=current_user_cursor,
               n_followers=current_user_n_followers,
               oauth=oauth, log_file="Data/Followers_lists/_log_Scrape_Followers.Rda")
  # error <- tryCatch(getFollowers(user_id=current_user_id, screen_name=current_user_screenname, cursor=current_user_cursor,
  #                                n_followers=current_user_n_followers,
  #                                oauth=oauth, log_file="Data/Followers_lists/_log_Scrape_Followers.Rda"), 
  #                   error=function(e) e)
  # if (inherits(error, 'error')) {
  #   message("** Error! On to the next one...")
  #   next
  # }
}
