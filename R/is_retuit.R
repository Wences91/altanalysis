#' is_retuit
#' 
#' @param consumer_key consumer key for Twitter API
#' @param consumer_secret consumer secret for Twitter API
#' @param access_token access token for Twitter API
#' @param access_secret access secret for Twitter API
#' @param tweets twitter status ids vector
#' @export
#' @importFrom httr oauth_app Token1.0 GET content
#' 

#api_outh <- function(consumer_key, consumer_secret,
#                     access_token, access_secret){
#  
#  app <- httr::oauth_app('twitter', key = consumer_key, secret = consumer_secret)
#  
#  twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
#                                      app = app,
#                                      credentials = list(oauth_token = access_token, oauth_token_secret = access_secret))
#  
#  if(httr::GET('https://api.twitter.com/1.1/account/settings.json', config(token = twitter_token))$status_code == 200){
#    return(twitter_token)
#  }else{
#    warning('Wrong credentials', call. = FALSE)
#    return(NULL)
#  }
#}

is_retuit <- function(tweets,
                      consumer_key, consumer_secret,
                      access_token, access_secret){
  
  app <- httr::oauth_app('twitter', key = consumer_key, secret = consumer_secret)
  
  twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                                      app = app,
                                      credentials = list(oauth_token = access_token, oauth_token_secret = access_secret))
  
  if(httr::GET('https://api.twitter.com/1.1/account/settings.json', config(token = twitter_token))$status_code == 200){
    
    tweets <- as.character(tweets)
    
    retweets <- data.frame(id = tweets,
                           is_retweet = NA,
                           stringsAsFactors = FALSE)
    
    sapply(tweets, function(x){
      
      cat('\r', x)
      
      # at first try to check if limit has been reached
      limit <- TRUE
      while(limit){
        
        tweet_split <- unlist(strsplit(x, '/'))
        quest <- httr::GET(paste('https://api.twitter.com/1.1/statuses/show.json?id=', tweet_split[length(tweet_split)], sep = ''), config(token = twitter_token))
        
        if(quest$status_code == 429){
          print('\n', 'Please wait 15 minutes')
          Sys.sleep(61*15)
        }else{
          limit <- FALSE
        }
      }
      
      if(quest$status_code == 200){
        retweets[which(retweets$id == x), 'is_retweet'] <<- ifelse(length(httr::content(quest)$retweeted_status) > 0, TRUE, FALSE)
      }
    })
    
    return(retweets)
    
    }else{
    warning('Wrong credentials', call. = FALSE)
    return(NULL)
  }
}
