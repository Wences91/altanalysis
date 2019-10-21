#' network
#' 
#' @param nodes data.frame of nodes
#' @param edges data.frame of edges
#' @param min Minimun mentions
#' @param co_min Minimun co-mentions
#' @export
#' @import dplyr
#' @import igraph
#' 

network <- function(nodes, edges, min = 1, co_min = 1){
  
  # First check
  tryCatch(
    expr = {
      if(!(is.numeric(min) | min %% 1 == 0 | min > 0)){
        return('Minimum mentions error')
      }
      if(!(is.numeric(co_min) | co_min %% 1 == 0 |co_min > 0)){
        return('Minimum co-occurrence error')
      }
      #if(!(all(c('Outlet or Author', 'Details Page URL') %in% colnames(mentions)))){
      #  return('Mentions data.frame error')
      #}
    },error=function(e){
      message(e)
      return(NULL)
    }
  )
  
  
  if(!all(c(edges$Source, edges$Target) %in% nodes$`Outlet or Author`)){
    message('Any nodes from edges table are not include in nodes table')
  }
  if(!all(nodes$`Outlet or Author` %in% c(edges$Source, edges$Target))){
    message('Any nodes from nodes table are not include in edges table')
  }
  
  # create the graph
  g <- igraph::graph_from_data_frame(d = edges, directed = FALSE, vertices = nodes)
  
  # calculate nodes degree
  degree <- igraph::degree(g, mode = 'total') # nodes include it
  
  # filter by node degree
  if(co_min > 1){
    g <- igraph::induced_subgraph(
      g, igraph::V(g)[which(degree >= co_min)]
    )
  }
  
  # filter by edge weight
  if(min > 1){
    g <- igraph::subgraph.edges(
      g, igraph::E(g)[which(E(g)$`Co-occurrences` >= min), delete.vertices = FALSE]
    )
  }
  
  # reduce to the main component
  g <- igraph::induced_subgraph(
    g, igraph::V(g)[igraph::components(g)$membership == which.max(components(g)$csize)]
  )
  
  # re-calculate nodes degree
  degree <- igraph::degree(g, mode = 'total')
  
  # layout
  #l <- igraph::layout.fruchterman.reingold(g)
  #l <- igraph::layout.kamada.kawai(g)
  l <- igraph::layout.auto(g)
  #l <- igraph::layout.graphopt(g)
  #l <- igraph::layout.davidson.harel(g)
  
  # clustering
  #cluster <- igraph::cluster_leading_eigen(as.undirected(g))
  cluster <- igraph::cluster_louvain(as.undirected(g))
  
  igraph::modularity(g, cluster$membership)
  
  return(list(graph = g, cluster = cluster, layout = l))
  
}
