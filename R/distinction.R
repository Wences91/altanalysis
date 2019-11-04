#' distinction
#' 
#' @param mentions data.frame of mentions
#' @param type 1 for social media actors and social media types, 2 for research output titles and page URL
#' @export
#' @importFrom dplyr mutate
#' 

distinction <- function(mentions, type){
  
  # First check
  tryCatch(
    expr = {
      if(!(type %in% c(1,2))){
        message('Type error')
        return(NULL)
      }else if(type == 1){
        if(!(all(c('Mention Type', 'Outlet or Author') %in% colnames(mentions)))){
          message('Mentions data.frame error')
          return(NULL)
        }
      }else if(type == 2){
        if(!(all(c('Research Output Title', 'Details Page URL') %in% colnames(mentions)))){
          message('Mentions data.frame error')
          return(NULL)
        }
        }
    },error=function(e){
      message(e)
      return(NULL)
    }
  )
  if(type == 1){
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
  # this only warns
  if(type == 2){
    duplicated_research <- unique(mentions[, which(names(mentions) %in% c('Research Output Title', 'Details Page URL'))])
    
    if(any(duplicated(duplicated_research$`Research Output Title`))){
      warning(paste(as.character(length(which(duplicated(duplicated_research$`Research Output Title`)))), 'Research Output Title are duplicated'), call. = FALSE)
    }else{
      message('Not duplicated Research Output Title')
    }
    if(any(duplicated(duplicated_research$`Details Page URL`))){
      warning(paste(which(as.character(length(duplicated(duplicated_research$`Details Page URL`)))), 'Details Page URL are duplicated'), call. = FALSE)
    }else{
      message('Not duplicated Details Page URL')
    }
    return(NULL)
  }
}

