#' co_words_annotatde
#' 
#' @param publications_titles titles of publications
#' @export
#' @import NLP
#' @import tm
#' @importFrom openNLP Maxent_Sent_Token_Annotator Maxent_Word_Token_Annotator

# recommendation
#options(java.parameters = "- Xmx3000m")

# function 1 (annotate) all)
co_words_annotate <- function(publications_titles){
  # clean data
  publications_titles <- unique(publications_titles[,which(names(publications_titles) %in% c('Research Output Title', 'Details Page URL'))])
  
  #check duplicated
  distinction(publications_titles, 2)
  
  # preprocess
  publications_titles$`Research Output Title` <- gsub("[^-0-9A-Za-z///,/./: ]", '', publications_titles$`Research Output Title`, perl = TRUE)
  publications_titles$`Research Output Title` <- gsub("web of science", 'WOS', ignore.case = TRUE, publications_titles$`Research Output Title`)
  
  # create an empty listo for the whole process
  annotate_titles <- list()
  
  message('Annotating')
  
  # identify all words in the titles
  sapply(1:dim(publications_titles)[1], function(i){
    
    # count
    cat('\r', i)
    
    # at first I transform the title to lower case
    text <- tolower(publications_titles$`Research Output Title`[i])
    
    tryCatch(
      {
        annotate_titles[[i]] <<- list(title = text,
                                      id = publications_titles$`Details Page URL`[i],
                                      annotate = NLP::annotate(text, list(openNLP::Maxent_Sent_Token_Annotator(), openNLP::Maxent_Word_Token_Annotator())))
      },error=function(e){
        message('No words found')
        annotate_titles[[i]] <<- list(title = text,
                                      id = publications_titles$`Details Page URL`[i],
                                      annotate = NA)
      }
    )
    gc()
  })
  
  gc()
  return(annotate_titles)
}
