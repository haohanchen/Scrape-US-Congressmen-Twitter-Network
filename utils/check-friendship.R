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

# screen_names = c("chris_bail","KingGary")

# Input a vector of user_ids or a vector of screen_names
getFriendship <- function(oauth, user_ids = NULL, screen_names = NULL, sleep = 0.5){
  ## Handling rate limit
  # Initialize
  current_oauth <- list(oauth=oauth, j=1, time_round_start=Sys.time())
  # Check rate limit and jump to the first available
  current_oauth <- handleRateLimit(current_oauth)
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  
  ## url to call
  url <- "https://api.twitter.com/1.1/friendships/show.json"

  if (!is.null(screen_names)){
    params <- list(source_screen_name = screen_names[1], target_screen_name = screen_names[2])
  }
  if (!is.null(user_ids)){
    params <- list(source_user_id = user_ids[1], target_user_id = user_ids[2], stringify_ids="true")
  }
  url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  Sys.sleep(sleep)
  ## trying to parse JSON data
  json.data <- jsonlite::fromJSON(url.data)
}
  
  