#' @export

mentions_stats <- function(mentions){
  # This function gets the Altmetric mentions data.frame
  
  # First check
  tryCatch(
    {
      if(!(all(c('Mention Type', 'Details Page URL') %in% colnames(mentions)))){
        return('Mentions data.frame error')
      }
    },error=function(e){
      return(error)
    }
  )
  
  full_table <- data.frame(row.names = names(table(mentions$`Mention Type`)))
  
  total_mentions <- data.frame(`Total mentions` = as.integer(table(mentions$`Mention Type`)),
                               row.names = names(table(mentions$`Mention Type`)),
                               check.names = FALSE,
                               stringsAsFactors = FALSE)
  
  unique_mentions <- data.frame(`Unique mentions` = as.integer(table(unique(mentions[,c('Mention Type', 'Details Page URL')])$`Mention Type`)),
                                row.names = names(table(mentions$`Mention Type`)),
                                check.names = FALSE,
                                stringsAsFactors = FALSE)
  
  full_table <- merge(total_mentions, unique_mentions, by='row.names', all = TRUE)
  # Fix rownames
  rownames(full_table) <- full_table$Row.names
  full_table <- full_table[,-which(colnames(full_table) == 'Row.names')]
  full_table$`Percentage` <- round(100 * full_table$`Total mentions` / sum(full_table$`Total mentions`), 3)
  
  return(full_table)
}
