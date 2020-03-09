#' co_keywords_network
#' 
#' @param keywords Keywords data.frame from altmetric_keywords
#' @param keywords_mentions Altmetric.com mentions with keywords from altmetric_keywords
#' @param binary Boolean that indactes if the occurrence count carried is binary or not
#' @param min_co To filter the network on the basis of a minimum of co-occurrences
#' @description This function gets the keywords and keywords mentions from altmetric_keywords and obtains a co-occurrence network of keywords. It returns a list wit two data.frame that correspond to the nodes and edges of the network.
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' 

co_keywords_network <- function(keywords, keywords_mentions, binary=TRUE, min_co=1){
  
  # Mention URL is needed to identify each unique mention and use the score
  keys_score <- keywords_mentions[,c('keyword', 'Mention URL', 'Outlet or Author')]
  
  # If binary (some tweets can mention more than one paper)
  if(binary){
    keys_score <- unique(keys_score)
  }
  
  # Network edges
  co_keywords <- dplyr::inner_join(keywords[,c('keyword', 'id')], keywords[,c('keyword', 'id')], by='id')
  
  ## Delete wrong co-kewords
  co_keywords <- co_keywords[which(!(co_keywords$keyword.x == co_keywords$keyword.y)),]
  
  co_keywords <- dplyr::mutate(co_keywords,
                               Source = pmin(keyword.x, keyword.y),
                               Target = pmax(keyword.x, keyword.y))
  
  co_keywords <- co_keywords[, which(!(names(co_keywords) %in% c('keyword.x', 'keyword.y')))]
  co_keywords <- dplyr::distinct(co_keywords, .keep_all = TRUE)
  
  ## Change data.frame names
  names(co_keywords)[names(co_keywords) == 'id'] <- 'Weight'
  
  ## Group and sum mentions
  co_keywords$Weight <- 1
  
  co_keywords <- dplyr::group_by(co_keywords, Source, Target)
  co_keywords <- dplyr::summarise(co_keywords, Weight = sum(Weight))
  
  co_keywords <- as.data.frame(co_keywords, stringsAsFactors = FALSE)
  co_keywords <- co_keywords[order(co_keywords$Weight, decreasing = TRUE),]
  
  ## Filter
  co_keywords <- co_keywords[which(co_keywords$Weight >= min_co),]
  
  
  # Network nodes
  nodes <- keys_score[,c('keyword', 'Mention URL')]
  names(nodes)[which(names(nodes) == 'Mention URL')] <- 'Weight'
  
  nodes$Weight <- 1
  nodes <- dplyr::group_by(nodes, keyword)
  nodes <- dplyr::summarise(nodes, Weight=sum(Weight))
  
  nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)
  nodes <- nodes[order(nodes$Weight, decreasing = TRUE),]
  
  names(nodes) <- c('id', 'occurrence')
  nodes$label <- nodes$id
  
  ## Only nodes with co-occurrences
  nodes <- nodes[which(nodes$id %in% unique(c(co_keywords$Source, co_keywords$Target))),]
  
  return(list(nodes=nodes, edges=co_keywords))
}
