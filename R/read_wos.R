#' read_wos
#' 
#' @param path directory path with all WoS records
#' @export
#' 

wos_read <- function(path){
  # Import full WoS records
  wos_records <- data.frame()
  
  sapply(dir(path), function(x){
    if(strsplit(x, '\\.')[[1]][2] == 'txt' & !is.na(strsplit(x, '\\.')[[1]][2])){
      wos_record <- read.delim(paste(path, '/', x, sep=''),  stringsAsFactors=FALSE,
                               quote = '',
                               row.names = NULL,)
      wos_records <<- rbind.data.frame(wos_records, wos_record, stringsAsFactors=FALSE)
    }
  })
  
  # Fix header
  if(names(wos_records)[1] == 'row.names'){
    names(wos_records) <- names(wos_records)[-1]
    wos_records <- wos_records[,-dim(wos_records)[2]]
  }
  
  return(wos_records)
}
