co_authors <- function(mentions){
  # This function gets the Altmetric mentions data.frame
  
  # First check
  tryCatch(
    {
      if(!(all(c('Outlet or Author', 'Details Page URL') %in% colnames(mentions)))){
        return('Mentions data.frame error')
      }
    },error=function(e){
      return(error)
    }
  )
  
  # reduce data.frame to two columns (ONLY UNIQUE)
  mentions <- unique(mentions[,which(names(mentions) %in% c('Outlet or Author', 'Details Page URL'))])
  
  # merge co-authors
  co_authors <- merge.default(x = mentions, y= mentions, by = 'Details Page URL')
  
  # delete same co-authors
  co_authors <- co_authors[which(!(co_authors$`Outlet or Author.x` == co_authors$`Outlet or Author.y`)),]
  co_authors_works <- data.frame('Details Page URL' = co_authors$`Details Page URL`,
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE)
  
  co_authors <- as.data.frame(t(apply(co_authors[,which(names(co_authors) %in% c('Outlet or Author.x', 'Outlet or Author.y'))], 1, sort)),
                              stringsAsFactors = FALSE)
  
  co_authors <- cbind.data.frame(co_authors, co_authors_works, stringsAsFactors = FALSE)
  #co_authors$Total <- 1
  #co_authors <- aggregate(Total ~ V1 + V2 + `Details Page URL`, co_authors, 'sum')
  
  co_authors <- co_authors[!duplicated(co_authors), ]
  return(co_authors)
}

