#' altmetric_wos_mentions
#' 
#' @param mentions Altmetric.com mentions data.frame
#' @param wos_incites InCites WoS records data.frame
#' @param filter Character or vector of optional Altmetric.com mention types
#' @param only_tweets Boolean indicating if want to reduce Twitter mentions to only tweets
#' @param retweets Tweet or retweet data.frame
#' @description This function joins Altmetric mentions with InCites WoS records, optionally can also filter by mention types and remove retweets. As result, it returns mentions with accession number and title from InCites.
#' @export
#' @importFrom dplyr inner_join
#' 

altmetric_wos_mentions <- function(mentions, wos_incites, filter=NULL, only_tweets=FALSE, retweets=NULL){
  
  # Filter
  ## Remove WoS records without DOI
  wos_incites <- wos_incites[which(!is.na(wos_incites$DOI)),]
  
  ## DOI to lower case in both datasets in order to link them correctly
  wos_incites$DOI <- tolower(wos_incites$DOI)
  mentions$DOI  <- tolower(mentions$DOI)
  
  # Are all Altmetric.com DOI in WoS dataset?
  if(!all(mentions$DOI %in% wos_incites$DOI)){
    stop('There are some Altmetric mentions not include in WoS records')
  }
  
  ## Select ducplicated DOI in WoS and remove them in WoS and Altmetric.com
  duplicated_doi <- wos_incites$DOI[which(duplicated(wos_incites$DOI))]
  if(length(duplicated_doi) > 0){
    warning(paste('There are', as.character(length(duplicated_doi)), 'records with duplicated DOI'), call.=FALSE)
    wos_incites <- wos_incites[which(!(wos_incites$DOI %in% duplicated_doi)),]
    mentions <- mentions[which(!(mentions$DOI %in% duplicated_doi)),]
  }
  
  ## Filter Altmetric.com mentions by type
  if(!is.null(filter)){
    mentions <- mentions[which(mentions$`Mention Type` %in% filter),]
  }
  
  ## Fix missing authors names (only in case of Twitter mentions)
  if(is.null(filter) | 'Tweet' %in% filter){
    empties <- which(mentions$`Outlet or Author`[which(mentions$`Mention Type` == 'Tweet')] == '' | is.na(is.na(mentions$`Outlet or Author`[which(mentions$`Mention Type` == 'Tweet')])))
    if(length(empties) > 0){
      warning(paste('There are', as.character(length(empties)), 'Twitter mentions without author name'), call.=FALSE)
      sapply(empties, function(x){
        mentions$`Outlet or Author`[which(mentions$`Mention Type` == 'Tweet')][x] <<- paste0('@', strsplit(mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')][x], '/|/#!/')[[1]][4])
      })
    }
  }
  
  ## Merge InCites records (only Accession Number and Title fields) with Altmetric.com mentions
  if(any(duplicated(wos_incites[,c('Accession Number')])) | any(duplicated(wos_incites[,c('DOI')]))){
    stop('There are some duplicated WoS records')
  }else{
    mentions <- dplyr::inner_join(mentions, wos_incites[,c('Accession Number', 'DOI', 'Article Title')], by='DOI')
  }
  
  ## Filter by only tweets
  if(only_tweets){
    if(all(mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')] %in% retweets$`Mention URL`)){
      message('Tweets: ', round(100*sum(retweets$Retweet[retweets$`Mention URL` %in% mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')]], na.rm = TRUE)/dim(retweets[retweets$`Mention URL` %in% mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')],])[1], 2))
      message('Retweets: ', round(100*sum((!retweets$Retweet[retweets$`Mention URL` %in% mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')]]), na.rm = TRUE)/dim(retweets[retweets$`Mention URL` %in% mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')],])[1], 2))
      message('Unknown: ', round(100*sum((is.na(retweets$Retweet[retweets$`Mention URL` %in% mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')]])), na.rm = TRUE)/dim(retweets[retweets$`Mention URL` %in% mentions$`Mention URL`[which(mentions$`Mention Type` == 'Tweet')],])[1], 2))
      mentions <- mentions[which(!(mentions$`Mention URL` %in% retweets$`Mention URL`[retweets$Retweet] | mentions$`Mention URL` %in% retweets$`Mention URL`[which(is.na(retweets$Retweet))])),]
    }else{
      stop('Some retweets are not included')
    }
  }
  
  return(mentions)
}
