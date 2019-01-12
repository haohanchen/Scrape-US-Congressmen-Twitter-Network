getLimitFriends <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "friends,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$friends$`/friends/ids`['remaining']))
}

getLimitRate <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}

getLimitFollowers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$followers$`/followers/ids`[['remaining']]))
}

getLimitUsers <- function(my_oauth){
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "users,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET",
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(jsonlite::fromJSON(response)$resources$users$`/users/lookup`[['remaining']]))
  
}

getLimitSearch <- function(my_oauth){
  require(rjson); require(ROAuth)
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "search")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$search$`/search/tweets`[['remaining']]))
}
