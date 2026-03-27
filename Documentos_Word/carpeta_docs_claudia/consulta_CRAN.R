#Función para unificar la consulta en CRAM
consulta_CRAN <- function(query) {
  if(grepl("\\||&", query)){
    return(query)
  }
  query <- gsub("\\s+or\\s+", "|", query, ignore.case= TRUE)
  query <- gsub("\\s+and\\s+", "&", query, ignore.case= TRUE)
  
  return(query)
}