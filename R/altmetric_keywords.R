#' altmetric_keywords
#' 
#' @param altmetric_wos_mentions Altmetric.com mentions with WoS data
#' @param wos_records WoS records
#' @param remove_journals name of journals to remove
#' @export
#' @importFrom dplyr inner_join
#' 


altmetric_keywords <- function(altmetric_wos_mentions, wos_records, remove_journals=NULL){
  # Check integrity
  if(!(all(wos_records$UT %in% altmetric_wos_mentions$`Accession Number`) & all(altmetric_wos_mentions$`Accession Number` %in% wos_records$UT))){
    stop('Some Altmetric or WoS records are not included')
  }
  
  # Create a empty data.frame to add al keywords
  keys <- data.frame(id=character(), keyword=character(), stringsAsFactors = FALSE)
  
  ### Select accession number (UT) and authors keywords (DE)

  # remove journals
  if(!is.null(remove_journals)){
    keywords <- wos_records[which(!(wos_records$SO %in% remove_journals)),]
  }
  keywords <- keywords[, c('UT','DE')]
  keywords <- keywords[which(keywords$DE!=''),]
  
  # keywords to lower to reduce problems
  keywords$DE <- tolower(keywords$DE)
  
  # split keywords
  sapply(1:dim(keywords)[1], function(x){
    aux <- strsplit(keywords$DE[x], '; ')[[1]]
    keys <<- rbind.data.frame(data.frame(id=keywords$UT[x], keyword=aux, stringsAsFactors = FALSE), keys,
                              stringsAsFactors = FALSE)
  })
  
  # remove 0 from web 2.0 and similars
  keys <- keys[which(!(keys$keyword == '0')),]
  
  # remove ' and "
  keys$keyword <- gsub( '"|\'', '', keys$keyword)
  
  # remove some duplicated keywords in the same records
  keys <- unique(keys)
  
  keys_mentions <- dplyr::inner_join(keys, altmetric_wos_mentions, by=c('id'='Accession Number'))
  
  return(list(keywords=keys, mentions=keys_mentions))
}
