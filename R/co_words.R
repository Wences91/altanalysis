#' co_words
#' 
#' @param title_words List generated in words extraction functions
#' @param min_oc Minimum occurrence
#' @param min_co Minimum co-occurrence
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' 

co_words <- function(title_words, min_oc=1, min_co=1){
  # At first select the terms with an occurrence greater than or equal to min_oc
  # Then filter the mention terms dataset and obtain their co-occurrences
  
  # filter by occurrences
  words_list <- title_words$occurrences$word[which(title_words$occurrences$occurrences >= min_oc)]
  co_occurrences <- title_words$mentions[which(title_words$mentions$keyword %in% words_list),]
  
  # get words co-occurrences by title
  co_occurrences <- dplyr::inner_join(x=co_occurrences, y=co_occurrences, by='Details Page URL')
  
  # delete self-co-occurrences
  co_occurrences <- co_occurrences[which(!(co_occurrences$keyword.x == co_occurrences$keyword.y)),]
  
  # remove symmetrical elements to go faster
  # firstly order columns values row by row adding them to two new columns
  co_occurrences <- dplyr::mutate(co_occurrences,
                                  Source = pmin(keyword.x, keyword.y),
                                  Target = pmax(keyword.x, keyword.y))
  
  # remove original columns
  co_occurrences <- co_occurrences[, which(!(names(co_occurrences) %in% c('keyword.x', 'keyword.y')))]
  
  # remove symetrical elements
  co_occurrences <- dplyr::distinct(co_occurrences, .keep_all = TRUE)
  
  # prepare data.frame names to count co-occurrences
  names(co_occurrences)[names(co_occurrences) == 'Details Page URL'] <- 'Weight'
  
  # give all co-occurrences the value 1 and then aggregate them
  co_occurrences$Weight <- 1
  co_occurrences <- dplyr::group_by(co_occurrences, Source, Target)
  co_occurrences <- dplyr::summarise(co_occurrences, Weight=sum(Weight))
  
  # transform into data.frame
  co_occurrences <- as.data.frame(co_occurrences, stringsAsFactors = FALSE)
  
  # filter by co-occurrence
  co_occurrences <- co_occurrences[which(co_occurrences$Weight >= min_co),]
  
  return(co_occurrences)
}
