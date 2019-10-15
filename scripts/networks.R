co_authors <- function(mentions, min = 1){
  # This function gets the Altmetric mentions data.frame and return the co-authors network by a two columns data.frame
  
  # First check
  tryCatch(
    expr = {
      if(!(is.numeric(min) | min %% 1 == 0 | min > 0)){
        return('Minimum co-occurrence error')
      }
      if(!(all(c('Outlet or Author', 'Details Page URL') %in% colnames(mentions)))){
        return('Mentions data.frame error')
        }
    },error=function(e){
      return(error)
    }
  )
  # firstly it needs to remove wrong values such as empty values
  # reduce data.frame to two columns
  mentions <- mentions[,which(names(mentions) %in% c('Outlet or Author', 'Details Page URL'))]
  
  # merge co-authors
  co_authors <- merge.default(x = mentions, y= mentions, by = 'Details Page URL')
  
  # delete wrong co-authors
  co_authors <- co_authors[which(!(co_authors$`Outlet or Author.x` == co_authors$`Outlet or Author.y`)),]
  
  # save paper URI
  co_authors_works <- data.frame('Details Page URL' = co_authors$`Details Page URL`,
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE)
  
  # from symmetric matrix to unsymmetric matrix
  co_authors <- as.data.frame(t(apply(co_authors[,which(names(co_authors) %in% c('Outlet or Author.x', 'Outlet or Author.y'))], 1, sort)),
                              stringsAsFactors = FALSE)
  # join paper URI
  co_authors <- cbind.data.frame(co_authors, co_authors_works, stringsAsFactors = FALSE)
  #co_authors$Total <- 1
  #co_authors <- aggregate(Total ~ V1 + V2 + `Details Page URL`, co_authors, 'sum')
  
  co_authors <- co_authors[!duplicated(co_authors), ]
  
  # change data.frame names
  names(co_authors)[names(co_authors) == 'V1'] <- 'Source'
  names(co_authors)[names(co_authors) == 'V2'] <- 'Target'
  names(co_authors)[names(co_authors) == 'Details Page URL'] <- 'Co-occurrences'
  
  co_authors$`Co-occurrences` <- 1
  co_authors <- aggregate(`Co-occurrences` ~ Source + Target, data = co_authors, 'sum')
  
  # filter if there is a minimum value
  if(min > 1){
    co_authors <- co_authors[which(co_authors$`Co-occurrences` >= min),]
  }
  return(co_authors)
}
