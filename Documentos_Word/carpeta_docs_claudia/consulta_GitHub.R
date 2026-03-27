#Función para unificar la consulta en GitHub
consulta_GitHub <- function(query) {
  if(grepl('^c\\(\\s*(\\".*?\\"\\s*(,\\s*\\".*?\\"\\s*)*)\\)$', query)){
    return(query)
  }
  query <- tolower(query)
  condiciones <- strsplit(query, "\\s+and\\s+")[[1]]
  lapply(condiciones, function(g) {
  palabras_busqueda <- trimws(strsplit(g, "\\s+or\\s+")[[1]])
  sapply(palabras_busqueda, function(x) if (grepl("\\s", x)) paste0('"', x, '"') else x)
  })
}