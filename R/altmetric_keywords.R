#' altmetric_keywords
#' 
#' @param altmetric_wos_mentions Altmetric.com mentions with acccession number data.frame
#' @param wos_records WoS records data.frame
#' @param remove_journals Character o vector of optional names of journals to remove from
#' @param wrong_words Character o vector of optional keywords to remove from
#' @description This function get Altmetric.com mentions (the result from altmetric_wos_mentions) and WoS records. Then it splits all WoS records into author keywords (one per record) and join with Altmetric.com mentions. It returns a list with two data.frame: all Altmetric.com mentions that have author keywords and these, and the same with records.
#' @export
#' @importFrom dplyr inner_join
#' @importFrom tidyr separate_rows
#' 


altmetric_keywords <- function(altmetric_wos_mentions, wos_records, remove_journals=NULL, wrong_words=NULL){
  
  # Check integrity
  if(!(all(wos_records$UT %in% altmetric_wos_mentions$`Accession Number`) & all(altmetric_wos_mentions$`Accession Number` %in% wos_records$UT))){
    stop('Some Altmetric or WoS records are not included')
  }
  
  # Create a empty data.frame to gather all keywords
  keys <- data.frame(id=character(), keyword=character(), stringsAsFactors = FALSE)

  # Remove journals
  if(!is.null(remove_journals)){
    keywords <- wos_records[which(!(wos_records$SO %in% remove_journals)),]
  }else{
    keywords <- wos_records
  }
  
  # Select accession number (UT) and authors keywords (DE)
  keywords <- keywords[, c('UT','DE')]
  keywords <- keywords[which(keywords$DE!=''),]
  
  # Keywords to lower to reduce problems
  keywords$DE <- tolower(keywords$DE)
  
  # Split keywords
  keys <- tidyr::separate_rows(keywords, DE, sep = '; ')
  names(keys) <- c('id', 'keyword')
  
  # Remove keywords
  if(!is.null(wrong_words)){
    keys <- keys[which(!(keys$keyword %in% wrong_words)),]
  }
  
  # Remove ' and "
  keys$keyword <- gsub( '"|\'', '', keys$keyword)
  
  # Remove some duplicated keywords in the same records
  keys <- unique(keys)
  
  keys_mentions <- dplyr::inner_join(keys, altmetric_wos_mentions, by=c('id'='Accession Number'))
  
  return(list(keywords=keys, mentions=keys_mentions))
}
