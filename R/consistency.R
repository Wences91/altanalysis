#' consistency
#'
#' @param mentions data.frame of mentions
#' @export
#' 

consistency <- function(mentions){
  # First check
  tryCatch(
    {
      if(!(all(c('Research Output Title', 'Details Page URL') %in% colnames(mentions)))){
        return('Mentions data.frame error')
      }
    },error=function(e){
      message(e)
      return(NULL)
    }
  )
  
  mentions <- unique(mentions[, which(names(mentions) %in% c('Research Output Title', 'Details Page URL'))])
  
  warning_1 <- duplicated(mentions$`Research Output Title`) | duplicated(mentions$`Research Output Title`, fromLast = TRUE)
  warning_2 <- duplicated(mentions$`Details Page URL`) | duplicated(mentions$`Details Page URL`, fromLast = TRUE) # the worst
  
  if(any(warning_1)){
    message(paste('There are', as.character(sum(warning_1)), 'repeated publication titles'))
  }
  if(any(warning_2)){
    message(paste('There are', as.character(sum(warning_2)), 'repeated resources'))
  }
  
  return(list(Titles = unique(mentions$`Research Output Title`[warning_1]), Resources = unique(mentions$`Details Page URL`[warning_2])))
}
