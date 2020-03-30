#' socio_semantic_network
#' 
#' @param co_authors_edges Edges of co_authors from co_authors_network
#' @param keywords_mentions_cluster Altmetric.com mentions with keywords data.frame from co_authors_network
#' @param mode Socio-semantic netowrk mode: (1) Clouds by keywords communities and nodes colors by co-authors communities, (2) Clouds by co-authors communities and nodes colors by keywords communities
#' @description This function obtains the socio-semantic network
#' @export
#' @importFrom igraph graph_from_data_frame V E degree cluster_louvain layout.auto
#' 

socio_semantic_network <- function(co_authors_edges, keywords_mentions_cluster, mode=1){
  g <- igraph::graph_from_data_frame(co_authors_edges, directed=FALSE)
  
  # Reduce to main component
  g <- giant_component(g)
  
  # Calculate nodes degree
  igraph::V(g)$degree <- igraph::degree(g)
  
  if(mode==1){
    clusters_authors <- igraph::cluster_louvain(g, weights = igraph::E(g)$Weights)
    message('Authors modularity (cloud): ', round(clusters_authors$modularity, 2))
    clusters_keywords <- thematic_clusters_ranked(keywords_mentions_cluster, g)
    message('Semantic modularity (nodes): ', round(clusters_keywords$clusters$modularity, 2))
    clusters_cloud <- clusters_authors
    
    # Assign colors to nodes
    if(any(clusters_keywords$clusters$membership == (max(unique(keywords_mentions_cluster$cluster))+1))){
      igraph::V(g)$Colors <- '#FFFFFFFF'
      igraph::V(g)$Colors[which(!(clusters_keywords$clusters$membership %in% (max(unique(keywords_mentions_cluster$cluster))+1)))] <- rainbow(length(unique(clusters_keywords$clusters$membership))-1)[clusters_keywords$clusters$membership[which(!(clusters_keywords$clusters$membership %in% (max(unique(keywords_mentions_cluster$cluster))+1)))]]
    }else{
      igraph::V(g)$Colors <- rainbow(length(unique(clusters_keywords$clusters$membership)))[clusters_keywords$clusters$membership]
    }
  }else if(mode==2){
    clusters_authors <- thematic_clusters_ranked(keywords_mentions_cluster, g)
    message('Semantic modularity (cloud): ', round(clusters_authors$clusters$modularity, 2))
    clusters_keywords <- igraph::cluster_louvain(g, weights = igraph::E(g)$Weights)
    message('Authors modularity (nodes): ', round(clusters_keywords$modularity, 2))
    clusters_cloud <- clusters_authors$clusters
    
    # Assign colors to nodes
    igraph::V(g)$Colors <- rainbow(length(unique(clusters_keywords$membership)))[clusters_keywords$membership]
    
  }else{
    stop('Mode ', mode, ' does not exist')
  }
  
  
  l <- igraph::layout.auto(g)
  
  plot(clusters_cloud, g,
       layout=l,
       mark.expand=0, mark.shape = 0,
       vertex.label=NA, mark.border = NA, vertex.size=2,
       col =NA, vertex.frame.color=NA,
       edge.size=0, edge.color = NA,
       axes=F)
  
  plot(g,
       layout=l,
       vertex.label.cex = .2, 
       vertex.color=igraph::V(g)$Colors, vertex.size=2, vertex.frame.color=NA,
       edge.width=.1, #edge.color=NA,
       add = TRUE)
}