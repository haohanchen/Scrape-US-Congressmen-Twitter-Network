handleRateLimit <- function(current_oauth, verbose = T, sleep = 1){
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  current_oauth$limit <- getLimitFollowers(my_oauth)
  if (verbose == T){
    message(paste0("Using account ", current_oauth$j, ". API call left = ", current_oauth$limit))
  }
  while (current_oauth$limit==0){
    if (current_oauth$j == 1){
      current_oauth$time_round_start <- Sys.time()
    }
    current_oauth$j <- current_oauth$j + 1
    # when reaching the end of the list of accounts
    if (current_oauth$j > length(oauth)){
      # t_sleep <- 890
      t_sleep <- max(920 - as.numeric(Sys.time()) + as.numeric(current_oauth$time_round_start), 0)
      message("\n****\nNow: ", Sys.time(), "\nRound Started: ", current_oauth$time_round_start, "\nSleep ", t_sleep, " sec.", "\n****\n")
      Sys.sleep(t_sleep)
      # Reset
      current_oauth$j <- 1
      my_oauth <- current_oauth$oauth[[current_oauth$j]]
      current_oauth$time_round_start <- Sys.time()
    }
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
    message(paste0("* Shift to account ", current_oauth$j))
    Sys.sleep(sleep)
    
    current_oauth$limit <- getLimitFollowers(my_oauth)
    message(current_oauth$limit, " API calls left")
  }
  return(current_oauth)
}

getFollowers <- function(screen_name=NULL, oauth, user_id=NULL, cursor, sleep=1, n_followers=-1, log_file="Data/Followers_lists/_log_Scrape_Followers.Rda"){
  ## Handling rate limit
  # Initialize
  current_oauth <- list(oauth=oauth, j=1, time_round_start=Sys.time())
  # Check rate limit and jump to the first available
  current_oauth <- handleRateLimit(current_oauth)
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  # Load log file
  load(file = log_file)
  ## url to call
  url <- "https://api.twitter.com/1.1/followers/ids.json"
  
  ## Calculate what has already been scraped
  if (paste0(screen_name, ".txt") %in% list.files("Data/Followers_lists")) {
    message("Counting scraped followers...")
    con <- file(paste0("Data/Followers_lists/", screen_name, ".txt"), open="r")
    batch <- 20000
    count <- 0
    while((linesread <- length(readLines(con, batch))) > 0 ) {
      count <- count + linesread
    }
    close(con)
  } else {
    count <- 0
  }
  
  ## empty list for followers
  followers_batch <- c()
  
  ## while there's more data to download...
  while (cursor!=0){
    ## making API call
    # if (!is.null(screen_name)){
    #   params <- list(screen_name = screen_name, cursor = cursor, stringify_ids="true")
    # }
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
      message(url.data)
      stop("error! Last cursor: ", cursor)
    }
    ## adding new IDS
    followers <- as.character(json.data$ids)
    followers_batch <- c(followers_batch, followers)
    # if (!is.null(file)){
    #   followers <- as.character(json.data$ids)
    #   writeLines(followers, con=con)
    # }
    ## previous cursor
    prev_cursor <- json.data$previous_cursor_str
    ## next cursor
    cursor <- json.data$next_cursor_str
    
    ## giving info
    count <- count + length(followers)
    message("Account: ", screen_name, " Progress: ", count, "/", n_followers)

    # Checkpoint saving and clearing up the vector
    if (length(followers_batch) >= 50000 | cursor == 0){
      con <- file(paste0("Data/Followers_lists/", screen_name, ".txt"), open = "a")
      writeLines(followers_batch, con)
      close(con)
      message("** Checkpoint saving **")
      # Change log file. mark the cursor at the check point
      Scrape_Followers_Progress$next_cursor[Scrape_Followers_Progress$ID == current_user_id] <- cursor
      save(Scrape_Followers_Progress, file = log_file)
      followers_batch <- c()
    }
    
    ## handle rate limit
    current_oauth <- handleRateLimit(current_oauth, verbose = F)
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
  }
  
}

