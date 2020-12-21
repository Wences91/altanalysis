#' socio_semantic_network
#' 
#' @param co_authors_edges Edges of co_authors from co_authors_network
#' @param keywords_mentions_cluster Altmetric.com mentions with keywords data.frame from co_authors_network
#' @param mode Socio-semantic netowrk mode: (1) Clouds by keywords communities and nodes colors by co-authors communities, (2) Clouds by co-authors communities and nodes colors by keywords communities
#' @param layout Name of layout algorithm (1-Auto, 2-Kamada-Kawai, 3-Fruchterman-Reingold, 4-DrL, 5-Nicely, 6-Components)
#' @param legend Ordered vector with the names of the clusters
#' @param nodes_size Vector with minimum and maximum value of the nodes
#' @param label_ratio Ratio of the size of the node labels
#' @param custom_colors_nodes Vector of colors for the nodes
#' @param custom_colors_clouds Vector of colors for the clouds
#' @description This function obtains the socio-semantic network
#' @export
#' @importFrom igraph graph_from_data_frame V E degree cluster_louvain layout.auto layout_with_kk layout_with_fr layout_with_drl layout_nicely layout_components
#' 

socio_semantic_network <- function(co_authors_edges,
                                   keywords_mentions_cluster,
                                   mode=1,
                                   layout=1,
                                   legend,
                                   nodes_size = c(1:10),
                                   label_ratio = 0.1,
                                   custom_colors_nodes = NULL,
                                   custom_colors_clouds = NULL){
  
  g <- igraph::graph_from_data_frame(co_authors_edges, directed=FALSE)
  
  # Reduce to main component
  g <- giant_component(g)
  
  # Calculate nodes degree
  igraph::V(g)$degree <- igraph::degree(g)
  igraph::V(g)$degree_norm <- (nodes_size[2]-nodes_size[1])*((igraph::V(g)$degree-min(igraph::V(g)$degree))/(max(igraph::V(g)$degree)-min(igraph::V(g)$degree)))+nodes_size[1]
  
  if(mode==1){
    clusters_authors <- igraph::cluster_louvain(g, weights = igraph::E(g)$Weights)
    message("Authors modularity (cloud): ", round(mean(clusters_authors$modularity), 2), ' | Clusters: ', length(unique(clusters_authors$membership)))
    clusters_keywords <- thematic_clusters_ranked(keywords_mentions_cluster, g)
    message("Semantic modularity (nodes): ", round(clusters_keywords$clusters$modularity, 2), ' | Clusters: ', length(unique(clusters_keywords$clusters$membership)))
    clusters_cloud <- clusters_authors
    clusters_nodes <- clusters_keywords$clusters
    
    igraph::V(g)$cloud <- clusters_authors$membership
    igraph::V(g)$freq <- clusters_keywords$distribution_top$freq_abs
    igraph::V(g)$percentage <- clusters_keywords$distribution_top$freq
    
    # Assign colors to nodes
    if(any(clusters_keywords$clusters$membership == (max(unique(keywords_mentions_cluster$cluster))+1))){
      igraph::V(g)$Colors <- '#FFFFFFFF'
      igraph::V(g)$Colors[which(!(clusters_keywords$clusters$membership %in% (max(unique(keywords_mentions_cluster$cluster))+1)))] <- rainbow(length(unique(clusters_keywords$clusters$membership))-1)[clusters_keywords$clusters$membership[which(!(clusters_keywords$clusters$membership %in% (max(unique(keywords_mentions_cluster$cluster))+1)))]]
    }else{
      if(!is.null(custom_colors_nodes)){
        igraph::V(g)$Colors <- custom_colors_nodes[clusters_keywords$clusters$membership]
      }else{
        igraph::V(g)$Colors <- rainbow(length(unique(clusters_keywords$clusters$membership)))[clusters_keywords$clusters$membership]
      }
    }
  }else if(mode==2){
    clusters_authors <- thematic_clusters_ranked(keywords_mentions_cluster, g)
    message("Semantic modularity (cloud): ", round(clusters_authors$clusters$modularity, 2), ' | Clusters: ', length(unique(clusters_authors$clusters$membership)))
    clusters_keywords <- igraph::cluster_louvain(g, weights = igraph::E(g)$Weights)
    message("Authors modularity (nodes): ", round(mean(clusters_keywords$modularity), 2), ' | Clusters: ', length(unique(clusters_keywords$membership)))
    clusters_cloud <- clusters_authors$clusters
    clusters_nodes <- clusters_keywords
    
    igraph::V(g)$cloud <- clusters_authors$clusters$membership
    igraph::V(g)$freq <- clusters_authors$distribution_top$freq_abs
    igraph::V(g)$percentage <- clusters_authors$distribution_top$freq
    
    # Assign colors to nodes
    if(!is.null(custom_colors_nodes)){
      igraph::V(g)$Colors <- custom_colors_nodes[(clusters_keywords$membership+1)]
    }else{
      igraph::V(g)$Colors <- rainbow(length(unique(clusters_keywords$membership)))[(clusters_keywords$membership+1)]
    }
    
    
  }else{
    stop('Mode ', mode, ' does not exist')
  }
  
  l <- switch(layout, igraph::layout.auto(g), igraph::layout_with_kk(g), igraph::layout_with_fr(g), igraph::layout_with_drl(g), igraph::layout_nicely(g), igraph::layout_components(g))
  
  if(!is.null(custom_colors_clouds)){
    custom_colors_clouds <- paste0(custom_colors_clouds, 30)
    
    plot(clusters_cloud, g, mark.col = custom_colors_clouds,
         layout=l,
         mark.expand=0, mark.shape = 0, vertex.label.color = 'black',
         vertex.label=NA, mark.border = NA, vertex.size=igraph::V(g)$degree_norm,
         col =NA, vertex.frame.color=NA,
         edge.size=0, edge.color = NA,
         axes=F)
    plot(g,
         layout=l,
         vertex.label.color = 'black', vertex.label.cex = igraph::V(g)$degree_norm*label_ratio, #vertex.label = NA
         vertex.color=paste0(igraph::V(g)$Colors,85), vertex.size=igraph::V(g)$degree_norm, vertex.frame.color=NA,
         edge.width=.1, edge.curved = 0.5, #edge.color=NA,
         add = TRUE)
    
    #legend('bottomright', legend, pch = 21, pt.bg = custom_colors_nodes[1:length(legend)], col = custom_colors_nodes[1:length(legend)], bty = 'n', title = expression(bold('Nodes')),
    #       y.intersp = 1, x.intersp = 0.5, xjust = 0,
    #       title.adj = 0.5, ncol = 1,
    #       cex = 0.9)
    
    #legend('bottomright', legend, pch = 21, pt.bg = rainbow(length(legend)), col = rainbow(length(legend)), bty = 'n', title = expression(bold('Nodes')),
    #       y.intersp = 1, x.intersp = 0.5, xjust = 0,
    #       title.adj = 0.5, ncol = 1,
    #       cex = 0.9)

  }else{
    plot(clusters_cloud, g,
         layout=l,
         mark.expand=0, mark.shape = 0, vertex.label.color = 'black',
         vertex.label=NA, mark.border = NA, vertex.size=igraph::V(g)$degree_norm,
         col =NA, vertex.frame.color=NA,
         edge.size=0, edge.color = NA,
         axes=F)
    plot(g,
         layout=l,
         vertex.label.color = 'black', vertex.label.cex = igraph::V(g)$degree_norm*label_ratio, #vertex.label = NA
         vertex.color=paste0(igraph::V(g)$Colors,85), vertex.size=igraph::V(g)$degree_norm, vertex.frame.color=NA,
         edge.width=.1, edge.curved = 0.5, #edge.color=NA,
         add = TRUE)
    
  }
  
  compare <- data.frame(nodes=clusters_nodes$membership, clouds=clusters_cloud$membership, count=1, stringsAsFactors = FALSE)
  compare <- dplyr::group_by(compare, nodes, clouds)
  compare <- dplyr::summarise(compare, count=sum(count))
  return(compare)
}
