handleRateLimit <- function(current_oauth){
  my_oauth <- current_oauth$oauth[[current_oauth$j]]
  current_oauth$limit <- getLimitUsers(my_oauth)
  message(paste0("\nUsing account ", current_oauth$j, ". API call left = ", current_oauth$limit,"\n"))
  
  while (current_oauth$limit < 800){
    current_oauth$j <- current_oauth$j + 1
    
    # when reaching the end of the list of accounts
    if (current_oauth$j > length(oauth)){
      t_sleep <- max(900 - as.numeric(Sys.time() - current_oauth$time_round_start), 0)
      message("\n****\n", Sys.time(), "\n Sleep ", t_sleep, " sec.", "\n****\n")
      Sys.sleep(t_sleep)
      # Reset
      current_oauth$j <- 1
      my_oauth <- current_oauth$oauth[[current_oauth$j]]
      current_oauth$time_round_start <- Sys.time()
    }
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
    message(paste0("* Shift to account ", current_oauth$j))
    Sys.sleep(1)
    
    current_oauth$limit <- getLimitUsers(my_oauth)
    message(current_oauth$limit, " API calls left")
  }
  return(current_oauth)
}



getUsersBatch <- function(ids=NULL, screen_names=NULL, my_oauth, include_entities="false",
                          verbose=TRUE, output=NULL, fname = "temp.Rda"){
  
  if (fname %in% list.files("Data")){
    load(paste0("Data/", fname))
    i <- length(users.df) + 1
  } else{
    users.df <- list()
    i <- 1
  }
  
  failed <- c()
  
  left.ids <- if (is.null(ids)) {
    screen_names[!screen_names %in% users.df$scraped_screen_names]
  } else {
    ids[!ids %in% users.df$scraped_ids]
  }
  
  #Check Point
  t_checkpoint <- Sys.time()
  message("CHECKPOINT:", t_checkpoint)
  
  ## Handling rate limit
  # Initialize
  current_oauth <- list(oauth=oauth, j=1, time_round_start=Sys.time())
  
  while (length(left.ids)>0){
    message(i, "--", length(left.ids), ' users left')
    ids.tmp <- left.ids[1:min(c(100, length(left.ids)))]
    message(paste(ids.tmp, collapse=" "))
    
    # Handling rate limit
    # Check rate limit and jump to the first available
    current_oauth <- handleRateLimit(current_oauth)
    my_oauth <- current_oauth$oauth[[current_oauth$j]]
    
    if (!is.null(ids)){
      error <- tryCatch(tmp <- getUsers( my_oauth, id = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    
    if (!is.null(screen_names)){
      error <- tryCatch(tmp <- getUsers( my_oauth,
                                         screen_names = ids.tmp, include_entities=include_entities),
                        error = function(e) e)
    }
    
    # if error is found, go to next loop iteration
    if (inherits(error, 'error')){ 
      save(users.df, file = paste0("Data/", fname))
      message(paste0(ids.tmp, ": Can't load. Skipped\n"))
      next 
    }
    
    # if (!is.null(output)){ out <- lapply(tmp, function(x) writeLines(jsonlite::toJSON(x), con=conn)) }
    
    users.df[[as.character(i)]] <- data.frame(
      id_str = unlist(lapply(tmp, '[[', 'id_str')),
      screen_name = unlist(lapply(tmp, '[[', 'screen_name')),
      name = unlist(lapply(tmp, '[[', 'name')),
      description = unlist(lapply(tmp, '[[', 'description')),
      followers_count = unlist(lapply(tmp, '[[', 'followers_count')),
      statuses_count = unlist(lapply(tmp, '[[', 'statuses_count')),
      friends_count = unlist(lapply(tmp, '[[', 'friends_count')),
      created_at = unlist(lapply(tmp, '[[', 'created_at')),
      location = unlist(lapply(tmp, '[[', 'location')),
      stringsAsFactors=F)
    # Track ID's scraped
    users.df$scraped_ids <- c(users.df$scraped_ids, users.df[[as.character(i)]]$id_str)
    users.df$scraped_screen_names <- c(users.df$scraped_screen_names, users.df[[as.character(i)]]$screen_name)
    
    i <- i + 1
    left.ids <- left.ids[left.ids %in% ids.tmp == FALSE]
    
    # Save data at check points
    if (i %% 20 == 0){
      message("... Checkpoint saving...")
      save(users.df, file = paste0("Data/", fname))
      t_checkpoint <- Sys.time()
      message("CHECKPOINT:", t_checkpoint)
    }
  }
  save(users.df, file = paste0("Data/", fname))
  # users.df <- do.call(rbind, users.df)
  
  # if (!is.null(output)){ close(conn) }
  return(users.df)
}

getUsers <- function(my_oauth, screen_names=NULL,
                     ids=NULL, include_entities="true", verbose=FALSE){
  
  ## url to call
  url <- "https://api.twitter.com/1.1/users/lookup.json"
  
  ## first API call
  if (!is.null(screen_names)){
    screen_names <- paste(screen_names, collapse=",")
    params <- list(screen_name = screen_names, include_entities=include_entities)
  }
  if (!is.null(ids)){
    ids <- paste(ids, collapse=",")
    params <- list(user_id=ids, include_entities=include_entities)
  }
  
  options("httr_oauth_cache"=FALSE)
  app <- httr::oauth_app("twitter", key = my_oauth$consumerKey,
                         secret = my_oauth$consumerSecret)
  credentials <- list(oauth_token = my_oauth$oauthKey, 
                      oauth_token_secret = my_oauth$oauthSecret)
  twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                                      app = app, credentials = credentials)
  
  query <- lapply(params, function(x) URLencode(as.character(x)))
  url.data <- httr::GET(url, query=query, httr::config(token=twitter_token))
  json.data <- httr::content(url.data)
  Sys.sleep(0.5)
  return(json.data)
}



