#' co_authors
#' 
#' @param mentions data.frame of mentions
#' @param social_media Vector of social media names
#' @export
#' @importFrom dplyr inner_join mutate distinct group_by summarise
#' 

co_authors <- function(mentions, social_media = NULL){
  # This function gets the Altmetric mentions data.frame and return the co-authors network by a two columns data.frame
  # if there is not social_media argument it uses all social media mentions
  
  social_medias <- c('Tweet', 'News story', 'Wikipedia', 'Facebook')
  
  # First check
  tryCatch(
    expr = {
      if(!(all(c('Outlet or Author', 'Details Page URL') %in% colnames(mentions)))){
        return('Mentions data.frame error')
      }
      if(!(is.null(social_media)) & !(all(social_media %in% social_medias))){
        return('Social media error')
      }
    },error=function(e){
      message(e)
      return(NULL)
    }
  )
  
  # firstly filter mentions by social media
  if(!is.null(social_media)){
    mentions <- mentions[which(mentions$`Mention Type` %in% social_media),]
  }
  
  # it needs to remove wrong values such as empty values
  # reduce data.frame to two columns
  mentions <- mentions[,which(names(mentions) %in% c('Mention Type', 'Outlet or Author', 'Details Page URL'))]
  
  # remove empty social media actors
  mentions <- mentions[which(mentions$`Outlet or Author` != ''),]
  
  
  # solve the problem of different social media with the same name
  mentions_distinction <- actor_distinction(mentions)
  if(!is.null(mentions_distinction)){
    mentions <- mentions_distinction
  }
  
  # save actors
  actors <- unique(mentions[, which(names(mentions) %in% c('Outlet or Author', 'Mention Type')),])
  actors$`Outlet or Author2` <- actors$`Outlet or Author` # it is needed to avoid problems with strange characters when they are merged
  
  # merge co-authors
  mentions <- mentions[, which(names(mentions) %in% c('Outlet or Author', 'Details Page URL')),]
  #co_authors <- merge.default(x = mentions, y= mentions, by = 'Details Page URL')
  co_authors <- dplyr::inner_join(x = mentions, y = mentions, by = 'Details Page URL') # faster than merge
  
  # delete wrong co-authors
  co_authors <- co_authors[which(!(co_authors$`Outlet or Author.x` == co_authors$`Outlet or Author.y`)),]
  
  
  # unique of two columns without order
  #co_authors <- as.data.frame(t(apply(co_authors[,which(names(co_authors) %in% c('Outlet or Author.x', 'Outlet or Author.y'))], 1, sort)),
  #                            stringsAsFactors = FALSE)
  #co_authors <- co_authors[!duplicated(co_authors), ]
  
  # faster way
  co_authors <- dplyr::mutate(co_authors,
                              Source = pmin(`Outlet or Author.x`, `Outlet or Author.y`),
                              Target = pmax(`Outlet or Author.x`, `Outlet or Author.y`))
  co_authors <- co_authors[, which(!(names(co_authors) %in% c('Outlet or Author.x', 'Outlet or Author.y')))]
  
  # remove
  co_authors <- dplyr::distinct(co_authors, .keep_all = TRUE)
  
  # change data.frame names
  names(co_authors)[names(co_authors) == 'Details Page URL'] <- 'Co-occurrences'
  
  # group and sum mentions
  co_authors$`Co-occurrences` <- 1
  co_authors <- dplyr::group_by(co_authors, Source, Target)
  co_authors <- dplyr::summarise(co_authors, `Co-occurrences` = sum(`Co-occurrences`))
  
  # mentions data.frame
  co_authors <- as.data.frame(co_authors, stringsAsFactors = FALSE)
  co_authors <- co_authors[order(co_authors$`Co-occurrences`, decreasing = TRUE),]
  
  # actors data.frame
  co_actors_table <- as.data.frame(table(c(co_authors$Source, co_authors$Target)),
                                   stringsAsFactors = FALSE)
  names(co_actors_table) <- c('Outlet or Author', 'Co-mentions')
  co_actors_table <- dplyr::inner_join(x = co_actors_table, y = actors, by = 'Outlet or Author')
  
  # fix problems with names when apply inner join using it as by
  co_actors_table <- co_actors_table[,-which(names(co_actors_table) == 'Outlet or Author')]
  names(co_actors_table)[which(names(co_actors_table) == 'Outlet or Author2')] <- 'Outlet or Author'
  
  # order columns
  co_actors_table <- co_actors_table[, c('Outlet or Author', 'Mention Type', 'Co-mentions')]
  
  # rename rownames
  rownames(co_authors) <- 1:dim(co_authors)[1]
  rownames(co_actors_table) <- 1:dim(co_actors_table)[1]
  
  return(list(mentions = co_authors,
              actors = co_actors_table))
}
