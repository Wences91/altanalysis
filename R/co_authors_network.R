#' co_authors_network
#' 
#' @param clusters A data.frame with all keywords and their clusters
#' @param keywords_mentions Altmetric.com mentions with keywords from altmetric_keywords
#' @param min_co To filter the network on the basis of a minimum of co-occurrences
#' @description This function gets the keywords and clusters data.frame and keywords mentions from altmetric_keywords and obtains a co-occurrence network of authors. It returns a list wit three data.frame: two of them correspond to the nodes and edges of the network and other one with mentions data.frame with clusters.
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' 

co_authors_network <- function(clusters, keywords_mentions, min_co=1){
  
  if(!(all(clusters$Id %in% keywords_mentions$keyword))){
    stop('Some keywords are not included')
  }
  
  # Merge cluster with mentions and keywords
  keywords_mentions_cluster <- dplyr::inner_join(keywords_mentions, clusters, by=c('keyword'='Id'))
  
  # Network edges
  ## Co-authors
  co_authors <- keywords_mentions_cluster[,c('keyword', 'Outlet or Author')]
  co_authors <- dplyr::inner_join(x = co_authors, y = co_authors, by = 'keyword')
  
  ## Delete wrong co-authors
  co_authors <- co_authors[which(!(co_authors$`Outlet or Author.x` == co_authors$`Outlet or Author.y`)),]
  
  co_authors <- dplyr::mutate(co_authors,
                              Source = pmin(`Outlet or Author.x`, `Outlet or Author.y`),
                              Target = pmax(`Outlet or Author.x`, `Outlet or Author.y`))
  co_authors <- co_authors[, which(!(names(co_authors) %in% c('Outlet or Author.x', 'Outlet or Author.y')))]
  co_authors <- dplyr::distinct(co_authors, .keep_all = TRUE)
  
  ## Change data.frame names
  names(co_authors)[names(co_authors) == 'keyword'] <- 'Weight'
  
  ## Group and sum mentions
  co_authors$Weight <- 1
  co_authors <- dplyr::group_by(co_authors, Source, Target)
  co_authors <- dplyr::summarise(co_authors, Weight = sum(Weight))
  
  
  co_authors <- as.data.frame(co_authors, stringsAsFactors = FALSE)
  co_authors <- co_authors[order(co_authors$Weight, decreasing = TRUE),]
  
  co_authors <- co_authors[which(co_authors$Weight >= min_co),]
  
  # Network nodes
  nodes <- c(co_authors$Source, co_authors$Target)
  nodes <- as.data.frame(table(nodes))
  
  return(list(keywords_mentions_cluster=keywords_mentions_cluster, nodes=nodes, edges=co_authors))
  
}
