#' two_mode_network
#' 
#' @param mentions data.frame of mentions
#' @export
#' @importFrom dplyr group_by summarise
#' 

two_mode_network <- function(mentions){
  # First check
  tryCatch(
    expr = {
      if(!(all(c('Mention Type', 'Research Output Title', 'Outlet or Author', 'Details Page URL') %in% colnames(mentions)))){
        return('Mentions data.frame error')
      }
    },error=function(e){
      message(e)
      return(NULL)
    }
  )
  
  # remover empty actor names instances
  mentions <- mentions[which(mentions$`Outlet or Author` != ''),]
  
  # firstly check titles and url
  distinction(mentions, 2)
  
  # solve the problem of different social media with the same name
  mentions_distinction <- distinction(mentions, type = 1)
  if(!is.null(mentions_distinction)){
    mentions[,c('Mention Type', 'Outlet or Author')] <- mentions_distinction[c('Mention Type', 'Outlet or Author')]
  }
  
  mentions <- mentions[,c('Mention Type', 'Research Output Title', 'Outlet or Author', 'Details Page URL')]
  
  # create edges data.frame
  edges <- mentions[,c('Outlet or Author', 'Details Page URL')]
  names(edges) <- c('Source', 'Target')
  edges$Mentions <- 1
  edges <- dplyr::group_by(edges, Source, Target)
  edges <- dplyr::summarise(edges, Mentions = sum(Mentions))
  edges <- as.data.frame(edges,
                         stringsAsFactors = FALSE)
  
  # create nodes data.frame
  mentions <- unique(mentions) # unique mentions
  nodes_author <- table(mentions$`Outlet or Author`)
  nodes_author <- data.frame(Id = names(nodes_author),
                             Label = names(nodes_author),
                             Mentions = as.integer(nodes_author),
                             stringsAsFactors = FALSE)
  nodes_author <- merge(nodes_author, unique(mentions[, c('Mention Type', 'Outlet or Author')]), by.x = 'Id', by.y = 'Outlet or Author')
  names(nodes_author)[which(names(nodes_author) == 'Mention Type')] <- 'Type'
  
  nodes_pub <- table(mentions$`Details Page URL`)
  nodes_pub <- data.frame(Id = names(nodes_pub),
                          Mentions = as.integer(nodes_pub),
                          Type = 'Publication',
                          stringsAsFactors = FALSE)
  nodes_pub <- merge(nodes_pub, unique(mentions[, c('Research Output Title', 'Details Page URL')]), by.x = 'Id', by.y = 'Details Page URL')
  names(nodes_pub)[which(names(nodes_pub) == 'Research Output Title')] <- 'Label'
  
  nodes <- rbind.data.frame(nodes_author, nodes_pub,
                            stringsAsFactors = FALSE)
  
  return(list(nodes = nodes,
              edges = edges))
}
