handleRateLimit <- function(current_oauth){
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  current_oauth$limit <- getLimitFriends(my_oauth)
  message(paste0("\nUsing account ", current_oauth$j, ". API call left = ", current_oauth$limit,"\n"))
  
  while (current_oauth$limit==0){
    current_oauth$j <- current_oauth$j + 1
    
    # when reaching the end of the list of accounts
    if (current_oauth$j > length(oauth)){
      t_sleep <- max(500 - as.numeric(Sys.time() - current_oauth$time_round_start), 0)
      message("\n****\n", Sys.time(), "\n Sleep ", t_sleep, " sec.", "\n****\n")
      Sys.sleep(t_sleep)
      # Reset
      current_oauth$j <- 1
      my_oauth <- current_oauth$oauth[[current_oauth$j]]
      current_oauth$time_round_start <- Sys.time()
    }
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
    message(paste0("* Shift to account ", current_oauth$j))
    Sys.sleep(0)
    
    current_oauth$limit <- getLimitFriends(my_oauth)
    message(current_oauth$limit, " API calls left")
  }
  return(current_oauth)
}

getFriends <- function(screen_name=NULL, oauth, cursor=-1, user_id=NULL, verbose=TRUE, sleep=0){
  ## Handling rate limit
  # Initialize
  current_oauth <- list(oauth=oauth, j=1, time_round_start=Sys.time())
  # Check rate limit and jump to the first available
  current_oauth <- handleRateLimit(current_oauth)
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  
  ## url to call
  url <- "https://api.twitter.com/1.1/friends/ids.json"
  ## empty list for friends
  friends <- c()
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, cursor = cursor, stringify_ids="true")
    }
    if (!is.null(user_id)){
      params <- list(user_id = user_id, cursor = cursor, stringify_ids="true")
    }
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    ## one API call less
    current_oauth$limit <- current_oauth$limit - 1
    ## trying to parse JSON data
    json.data <- jsonlite::fromJSON(url.data)
    if (length(json.data$error)!=0){
      if (verbose){message(url.data)}
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    friends <- c(friends, as.character(json.data$ids))

    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    ## giving info
    message(length(friends), " friends. Next cursor: ", cursor)

    ## changing oauth token if we hit the limit
    if (verbose){message(current_oauth$limit, " API calls left")}
    if (current_oauth$limit == 0){
      current_oauth <- handleRateLimit(current_oauth)
      my_oauth <- current_oauth$oauth[[current_oauth$j]]
    }
  }
  return(friends)
}


getFriendsDetails <- function(screen_name=NULL, oauth, cursor=-1, user_id=NULL, verbose=TRUE, sleep=0){
  ## Handling rate limit
  # Initialize
  current_oauth <- list(oauth=oauth, j=1, time_round_start=Sys.time())
  # Check rate limit and jump to the first available
  current_oauth <- handleRateLimit(current_oauth)
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  
  ## url to call
  url <- "https://api.twitter.com/1.1/friends/ids.json"
  ## empty list for friends
  friends <- c()
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, cursor = cursor, stringify_ids="true")
    }
    if (!is.null(user_id)){
      params <- list(user_id = user_id, cursor = cursor, stringify_ids="true")
    }
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    ## one API call less
    current_oauth$limit <- current_oauth$limit - 1
    ## trying to parse JSON data
    json.data <- jsonlite::fromJSON(url.data)
    if (length(json.data$error)!=0){
      if (verbose){message(url.data)}
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    friends <- c(friends, as.character(json.data$ids))
    
    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    ## giving info
    message(length(friends), " friends. Next cursor: ", cursor)
    
    ## changing oauth token if we hit the limit
    if (verbose){message(current_oauth$limit, " API calls left")}
    if (current_oauth$limit == 0){
      current_oauth <- handleRateLimit(current_oauth)
      my_oauth <- current_oauth$oauth[[current_oauth$j]]
    }
  }
  return(friends)
}
