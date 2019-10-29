#' co_words_tags
#' 
#' @param annotate_titles list with titles and words annotations
#' @export
#' @import NLP
#' @import tm
#' @importFrom openNLP Maxent_POS_Tag_Annotator

# recommendation
#options(java.parameters = "- Xmx3000m")

# function 2 (tag all)
co_words_tags <- function(annotate_titles){
  # assing tags to each word
  invisible(sapply(1:length(annotate_titles), function(i){
    print(i)
    # get tags
    pos_res <- NLP::annotate(annotate_titles[[i]]$title, openNLP::Maxent_POS_Tag_Annotator(language = 'en'), annotate_titles[[i]]$annotate)
    word_subset <- subset(pos_res, type == 'word')
    tags <- sapply(word_subset$features , '[[', "POS")
    
    annotate_titles[[i]]$tagged_words <<- data.frame(word = as.character(),
                                                     tag = as.character(),
                                                     stringsAsFactors = FALSE)
    
    # add word-tags
    sapply(1:length(word_subset), function(x){
      word <- substr(annotate_titles[[i]]$title, word_subset$start[x], word_subset$end[x])
      tag <- tags[x]
      annotate_titles[[i]]$tagged_words <<- rbind.data.frame(annotate_titles[[i]]$tagged_words, data.frame(word = word,
                                                                                                          tag = tag,
                                                                                                          stringsAsFactors = FALSE),
                                                             stringsAsFactors = FALSE)
    })
    
    gc()
  }))
  
  return(annotate_titles)
}
