#' profiles
#' 
#' @param clusters Clusters data.frame
#' @param legend Ordered vector with the names of the clusters
#' @param keywords_mentions Altmetric.com mentions with keywords from altmetric_keywords
#' @param nintersects Number of intersects
#' @param nsets Number of sets
#' @description 
#' @export
#' @importFrom dplyr right_join
#' @importFrom UpSetR upset
#' 

profiles <- function(clusters, legend, keywords_mentions, nsets = 6, nintersects = NA){
  
  # query for colors
  query <- list()
  for (color_q in legend){
    query <- append(query, list(list(query = intersects, params = list(color_q), color = '#3498db', active = T)))
  }
  
  clusters$cluster <-  setNames(legend, unique(sort(clusters$cluster)))[as.character(clusters$cluster)]
  
  setup <- dplyr::right_join(keywords_mentions$mentions, clusters, by = c(keyword = 'Id'))
  setup <- setup[, c('Outlet or Author', 'cluster')]
  setup$cluster <- as.character(setup$cluster)
  
  # first setup
  setup_2 <- as.data.frame(matrix(0, ncol = length(unique(setup$cluster)) + 1, nrow = length(unique(setup$`Outlet or Author`))))
  names(setup_2) <- c('Author', sort(unique(setup$cluster)))
  setup_2$Author <- unique(setup$`Outlet or Author`)
  
  message('There are ',length(setup_2$Author), ' authors')
  
  # check number of cluster mentions by author
  for (author in setup_2$Author){
    for (cluster in unique(setup$cluster[which(setup$`Outlet or Author` == author)])){
      setup_2[which(setup_2$Author == author), cluster] <- sum(setup$cluster[which(setup$`Outlet or Author` == author)] == cluster)
    }
  }
  
  # get percentages by author
  for (row in 1:dim(setup_2)[1]){
    setup_2[row,-1]  <- 100*setup_2[row,-1]/sum(setup_2[row,-1])
  }
  
  # setup general
  setup_g <- setup_2
  for (row in 1:dim(setup_g)[1]){
    setup_g[row,-1]  <- ifelse(setup_g[row,-1] > 0, 1, 0)
  }
  
  general <- UpSetR::upset(setup_g, main.bar.color = 'black', nsets = nsets, nintersects = nintersects,
                           order.by = 'freq',
                           queries = query,
                           point.size = 3.5, line.size = 2, 
                           text.scale = c(1.3, 1.3, 1, 1, 1.5, 1))
  
  
  for (i in c(5, 10, 15, 20)){
    setup_g <- setup_2
    for (row in 1:dim(setup_g)[1]){
      setup_g[row,-1]  <- ifelse(setup_g[row,-1] >= i, 1, 0)
    }
    if (i == 5){
      p5 <- UpSetR::upset(setup_g, main.bar.color = 'black', nsets = nsets, nintersects = nintersects,
                          order.by = 'freq',
                          queries = query,
                          point.size = 3.5, line.size = 2, 
                          text.scale = c(1.3, 1.3, 1, 1, 1.5, 1))
    }
    
    else if (i == 10){
      p10 <- UpSetR::upset(setup_g, main.bar.color = 'black', nsets = nsets, nintersects = nintersects,
                           order.by = 'freq',
                           queries = query,
                           point.size = 3.5, line.size = 2, 
                           text.scale = c(1.3, 1.3, 1, 1, 1.5, 1))
    }
    else if (i == 15){
      p15 <- UpSetR::upset(setup_g, main.bar.color = 'black', nsets = nsets, nintersects = nintersects,
                           order.by = 'freq',
                           queries = query,
                           point.size = 3.5, line.size = 2, 
                           text.scale = c(1.3, 1.3, 1, 1, 1.5, 1))
    }
    else if (i == 20){
      p20 <- UpSetR::upset(setup_g, main.bar.color = 'black', nsets = nsets, nintersects = nintersects,
                           order.by = 'freq',
                           queries = query,
                           point.size = 3.5, line.size = 2, 
                           text.scale = c(1.3, 1.3, 1, 1, 1.5, 1))
    }
    
  }
  return(list(general = general, p5 = p5, p10 = p10, p15 = p15, p20 = p20))
}
