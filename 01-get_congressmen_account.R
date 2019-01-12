#==============================================================================
# 01-get-congressmen-account
# Purpose: Download list of Congressmen
# Author: Haohan Chen
#==============================================================================

rm(list=ls())
# Get oauth
#oauth_keys <- read.csv("Data/Oauth_keys.csv", stringsAsFactors = F)
# it is possible that old file is corrupted so we load the backup
oauth_keys <- read.table("Data/oauth_keys copy.csv", sep='\t', stringsAsFactors = F, header=TRUE)
oauth <- list()
for (i in 1:nrow(oauth_keys)){
  oauth[[i]] <- ROAuth::OAuthFactory$new(consumerKey=oauth_keys$consumerKey[i],
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



# Need to improve on handling rate limit... haven't found the official API for this
handleRateLimit <- function(current_oauth){
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  message(paste0("\n\nUse account ", current_oauth$j, "\n\n"))
  while (current_oauth$limit==0){
    current_oauth$j <- current_oauth$j + 1
    # when reaching the end of the list of accounts
    if (j > length(all_oauth)){
      t_sleep <- max(900 - as.numeric(Sys.time() - current_oauth$time_round_start), 0)
      message("\n****\n", Sys.time(), "\n Sleep ", t_sleep, " sec.", "\n****\n")
      Sys.sleep(t_sleep)
      # Reset
      current_oauth$j <- 1
      current_oauth$time_round_start <- Sys.time()
    }
    message(paste0("\n\nShift to account ", current_oauth$j, "\n\n"))
    
    Sys.sleep(1)
    current_oauth$limit <- 75
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
    message(limit, " API calls left")
  }
  return(current_oauth)
}


Get_List_Members <- function(page_id){
  url <- "https://api.twitter.com/1.1/lists/members.json"
  # Initialize oauth
  current_oauth <- list(oauth = oauth, j = 1, time_round_start = Sys.time(), limit=75)
  # Check rate limit and jump to the first available
  current_oauth <- handleRateLimit(current_oauth)
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  
  # Initialize member list
  members_info <- list()
  
  cursor <- -1
  i <- 1
  while (cursor!=0){
    params <- list(list_id = page_id, cursor = cursor)
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    current_oauth$limit <- current_oauth$limit - 1
    url.data <- gsub("\n", " ", url.data)
    url.data <- gsub("\r", " ", url.data)
    url.data <- gsub("9\\11", "911", url.data, fixed=T)
    json.data <- jsonlite::fromJSON(url.data)
    users <- json.data$users[c("id", "id_str", "name", "screen_name", "location", "description", "url", "followers_count", "friends_count")]
    members_info[[i]] <- users
    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    
    count <- i * 20
    message(count, " members. Next cursor: ", cursor)
    
    ## handle rate limit
    current_oauth <- handleRateLimit(current_oauth)
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
    i <- i + 1
  }
  
  d <- do.call(rbind.data.frame, members_info)
  return(d)
}


#create "getfriendship" function to see if two users are connected on twitter,
#which we will use in a later step to create adjacency matrix
#this is the GET request we want:
#https://dev.twitter.com/rest/reference/get/friendships/show
#Chris did not finish this on 7/19 :(

# Input a vector of user_ids or a vector of screen_names
getFriendship <- function(user_ids = NULL, screen_names = NULL, sleep = 0.5){
  ## Handling rate limit
  # Initialize
  current_oauth <- list(source_id = NULL, target_id = NULL)
  # Check rate limit and jump to the first available
  current_oauth <- handleRateLimit(current_oauth)
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  
  ## url to call
  url <- "https://api.twitter.com/1.1/friendships/show.json"
  
  if (!is.null(screen_name)){
    params <- list(source_screen_name = screen_names[1], target_screen_name = screen_names[2], cursor = cursor)
  }
  if (!is.null(user_id)){
    params <- list(source_user_id = user_ids[1], target_user_id = user_ids[2], cursor = cursor, stringify_ids="true")
  }
  url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  Sys.sleep(sleep)
  ## trying to parse JSON data
  json.data <- jsonlite::fromJSON(url.data)
}



#pull data about congresspeople


Account_Info <- list()

# CSPN Members of congress
# https://twitter.com/cspan/lists/members-of-congress/members?lang=en&lang=en
Congressmen <- Get_List_Members("34179516")

# CSPN Governors
# https://twitter.com/cspan/lists/members-of-congress/members?lang=en&lang=en
Governors <- Get_List_Members("7560205")

write.csv(Congressmen, "Data/Congressmen.csv")
write.csv(Governors, "Data/Governors.csv", na="")




