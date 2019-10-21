#' actor_distinction
#' 
#' @param mentions data.frame of mentions
#' @export
#' @importFrom dplyr mutate
#' 

actor_distinction <- function(mentions){
  
  # First check
  tryCatch(
    expr = {
      if(!(all(c('Mention Type', 'Outlet or Author') %in% colnames(mentions)))){
        return('Mentions data.frame error')
      }
    },error=function(e){
      message(e)
      return(NULL)
    }
  )
  
  # solve the problem of different social media with the same name
  duplicated_media <- unique(mentions[, which(names(mentions) %in% c('Mention Type', 'Outlet or Author'))])
  duplicated_media <- duplicated_media$`Outlet or Author`[which(duplicated(duplicated_media$`Outlet or Author`))]
  
  if(length(duplicated_media) != 0){
    message(paste('Solving', as.character(length(duplicated_media)), 'duplicated names'))
    duplicated_media <- which(mentions$`Outlet or Author` %in% duplicated_media)
    mentions[duplicated_media,] <- dplyr::mutate(mentions[duplicated_media,],
                                                 `Outlet or Author` = paste(`Outlet or Author`, ' (', `Mention Type`, ')', sep = ''))
    return(mentions)
    }else{
    message('Not duplicated actor names')
    return(NULL)
  }
  
}

