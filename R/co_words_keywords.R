#' co_words_keywords
#' 
#' @param binary if binary counting TRUE
#' @param annotate_titles list with titles and words annotations
#' @export
#' @importFrom dplyr summarise group_by
#' @importFrom SemNetCleaner singularize
#' @importFrom udpipe as_phrasemachine

# recommendation
#options(java.parameters = "- Xmx3000m")

# function 3 (get keywords)
co_words_keywords <- function(annotate_titles, binary = TRUE){
  
  # prepare two data.frames
  stats <- data.frame(keyword = character(),
                      `Details Page URL` = character(),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  
  occurrence_table <- data.frame(word = character(),
                                 occurrences = character(),
                                 stringsAsFactors = FALSE)
  
  sapply(1:length(annotate_titles), function(x){
    
    cat('\r', x)
    
    if(all(is.na(annotate_titles[[x]]$tagged_words))){
      message('NA detected')
    }else{
      # a trick to avoid warnings
      annotate_titles[[x]]$tagged_words$tag[which(grepl('[^A-Z]', annotate_titles[[x]]$tagged_words$tag))] <<- 'POS'
      annotate_titles[[x]]$tagged_words$phrase_tag <<- udpipe::as_phrasemachine(annotate_titles[[x]]$tagged_words$tag, type = 'penn-treebank')
      
      stats_aux_0 <- udpipe::keywords_phrases(x = annotate_titles[[x]]$tagged_words$phrase_tag, term = annotate_titles[[x]]$tagged_words$word, 
                                              pattern = '(A|N)*N$',
                                              ngram_max = 4, # more?
                                              is_regex = TRUE, detailed = TRUE)
      
      # if doesn't find anything
      if(dim(stats_aux_0)[1] == 0){
        message(paste('Error', annotate_titles[[x]]$id))
      }else{
        # if there aren't problems add all words
        stats_aux_0 <- stats_aux_0[order(stats_aux_0$ngram, decreasing = TRUE),]
        
        # at first select the longest word
        if(length(unlist(strsplit(stats_aux_0$keyword[1], ' '))) > 1){
          # singularize the last word
          stats_aux <- paste(c(unlist(strsplit(stats_aux_0$keyword[1], ' '))[1:(max(length(unlist(strsplit(stats_aux_0$keyword[1], ' '))))-1)],
                               SemNetCleaner::singularize(unlist(strsplit(stats_aux_0$keyword[1], ' '))[max(length(unlist(strsplit(stats_aux_0$keyword[1], ' '))))])), collapse = ' ')
        }else{
          stats_aux <- SemNetCleaner::singularize(stats_aux_0$keyword[1])
        }
        
        index_words <- stats_aux_0$start[1]:stats_aux_0$end[1]
        
        # select the noun phrases
        if(dim(stats_aux_0)[1] > 1){
          sapply(2:dim(stats_aux_0)[1], function(x){
            if(!(all(c(stats_aux_0$start[x]:stats_aux_0$end[x]) %in% index_words))){
              # singularize the last word
              if(length(unlist(strsplit(stats_aux_0$keyword[x], ' '))) > 1){
                stats_aux <<- c(stats_aux,
                                paste(c(unlist(strsplit(stats_aux_0$keyword[x], ' '))[1:(max(length(unlist(strsplit(stats_aux_0$keyword[x], ' '))))-1)],
                                        SemNetCleaner::singularize(unlist(strsplit(stats_aux_0$keyword[x], ' '))[max(length(unlist(strsplit(stats_aux_0$keyword[x], ' '))))])), collapse = ' '))
              }else{
                stats_aux <<- c(stats_aux, SemNetCleaner::singularize(stats_aux_0$keyword[x]))
              }
              
              index_words <<- sort(c(index_words, c(stats_aux_0$start[x]:stats_aux_0$end[x])))
            }
          })
        }
        
        # whether binary
        if(binary){
          stats_aux <- unique(stats_aux) 
        }
        
        # occurrences
        occurrence_table <<- rbind.data.frame(occurrence_table, data.frame(word = stats_aux,
                                                                           occurrences = 1,
                                                                           stringsAsFactors = FALSE),
                                              stringsAsFactors = FALSE)
        occurrence_table <<- dplyr::group_by(occurrence_table, word)
        occurrence_table <<- dplyr::summarise(occurrence_table, occurrences = sum(occurrences))
        
        # word identification
        stats_aux <- data.frame(keyword = stats_aux, `Details Page URL` = rep(annotate_titles[[x]]$id, length(stats_aux)),
                                stringsAsFactors = FALSE,
                                check.names = FALSE)
        stats <<- rbind.data.frame(stats, stats_aux, stringsAsFactors = FALSE)
      }
    }
    })
  
  return(list(occurrences = as.data.frame(occurrence_table),
              mentions = stats))
}
