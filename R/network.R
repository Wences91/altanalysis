#' network
#' 
#' @param nodes data.frame of nodes
#' @param edges data.frame of edges
#' @param min Minimun mentions
#' @param co_min Minimun co-mentions
#' @export
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
  l <- igraph::layout.fruchterman.reingold(g)
  #l <- igraph::layout.kamada.kawai(g)
  #l <- igraph::layout.auto(g)
  #l <- igraph::layout.graphopt(g)
  #l <- igraph::layout.davidson.harel(g)
  
  # clustering
  #cluster <- igraph::cluster_leading_eigen(as.undirected(g))
  cluster <- igraph::cluster_louvain(as.undirected(g))
  
  message(paste('Q:', as.character(round(igraph::modularity(g, cluster$membership), 2))))
  
  colors <- rainbow(max(membership(cluster)), alpha = .7)
  
  #color_edges <- setNames(as.integer(membership(cluster)), names(membership(cluster)))
  
  # show top nodes
  top_limit <- as.integer(sort(degree, decreasing = TRUE)[5])
  
  network_plot <- plot(cluster, col = colors[membership(cluster)],
                       g, layout = l, rescale = FALSE, ylim=c(min(l[,2]), max(l[,2])), xlim = c(min(l[,1]), max(l[,1])),
                       edge.curved = .3, edge.width = igraph::E(g)$`Co-occurrences` / 10, edge.arrow.size=.05, edge.arrow.width=.2, edge.color = adjustcolor('grey10', alpha = .5),
                       #colors[color_edges[edges$Source]],
                       #vertex.color = colors[membership(cluster)],
                       vertex.frame.color = NA, vertex.size = degree * 2,
                       vertex.label = NA,
                       vertex.label.family = 'Arial',
                       vertex.label.cex=.5, vertex.label.color = 'black',
                       #asp = 0,
                       axes = FALSE)
  
  #return(list(graph = g, cluster = cluster, layout = l))
  
}
