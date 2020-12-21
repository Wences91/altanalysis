#' thematic_clusters_ranked
#' 
#' @param mentions A data.frame of mentions
#' @param nodes A network
#' @export
#' @importFrom igraph V make_clusters
#' 


thematic_clusters_ranked <- function(mentions, nodes){
  cluster_levels <- unique(sort(mentions$cluster))
  freq_dist_top <- data.frame(cluster=character(0), freq=integer(0), stringsAsFactors = FALSE)
  
  cluster <- matrix(integer(0), ncol = length(cluster_levels))
  cluster <- data.frame(cluster, stringsAsFactors = FALSE)
  names(cluster) <- cluster_levels
  
  other_cluster <- as.character(max(unique(mentions$cluster))+1)
  
  sapply(1:length(igraph::V(nodes)$name), function(x){
    
    # get the most common thematic cluster
    cluster_id <- table(factor(mentions[which(mentions$`Outlet or Author` == igraph::V(nodes)$name[x]), 'cluster'], levels = cluster_levels, ordered = TRUE))
    cluster_id <- as.data.frame(cluster_id, stringsAsFactors = FALSE)
    cluster_id <- data.frame(t(cluster_id), stringsAsFactors = FALSE)
    names(cluster_id) <- cluster_id[1,]
    cluster_id <- cluster_id[2,]
    
    cluster <<- rbind.data.frame(cluster, cluster_id, stringsAsFactors = FALSE)
  })
  
  cluster_aux <- cluster
  
  sapply(names(cluster), function(x){
    cluster[,x] <<- as.integer(cluster[,x])
    cluster_aux[,x] <<- as.integer(cluster_aux[,x])
    cluster[,x] <<- rank(-cluster[,x])
  })
  
  
  final_cluster <- character(0)
  # by row
  sapply(1:dim(cluster)[1], function(x){
    top_cluster <- colnames(cluster)[which(cluster[x,]==min(cluster[x,]))]
    freq_dist_top <<- rbind.data.frame(freq_dist_top,
                                       data.frame(cluster=top_cluster,
                                                  freq_abs = as.integer(cluster_aux[x, top_cluster]),
                                                  freq=100*as.integer(cluster_aux[x, top_cluster])/sum(cluster_aux[x,]), stringsAsFactors = FALSE),
                                       stringsAsFactors = FALSE)
    
    if(length(top_cluster) > 1){
      final_cluster <<- c(final_cluster, other_cluster)
    }else{
      final_cluster <<- c(final_cluster, top_cluster)
    }
  })
  
  
  clusters <- igraph::make_clusters(nodes, membership = as.integer(final_cluster), algorithm = NULL,
                                    merges = NULL, modularity = TRUE)
  
  return(list(clusters=clusters, distribution_top=freq_dist_top))
}