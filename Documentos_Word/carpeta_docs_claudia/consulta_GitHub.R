#Función para consulta en GitHub
consulta_GitHub <- function(query=NULL) {

   #Excepción para caso de llamada a función sin query
  if (base::is.null(query) || !base::is.character(query) || base::trimws(query) == "") {
    stop("Necesitas entregar un query a la función para realizar el filtrado")
  }

  # Convertir la query al formato GitHub si no lo está ya
  if(!base::grepl('^c\\(\\s*(\\".*?\\"\\s*(,\\s*\\".*?\\"\\s*)*)\\)$', query)){
    condiciones_AND <- base::strsplit(query, "and")[[1]]
    condiciones_AND <- base::trimws(condiciones_AND)

    condiciones_OR <- base::lapply(condiciones_AND, function(g) {
      terms <- base::strsplit(g, "or")[[1]]
      base::trimws(terms)
    })
  }
  # El query equivalente se ha de realizar combinando los strings del OR y uniéndolos luego (quivalencia con el AND)
  combinaciones <- base::expand.grid(condiciones_OR, stringsAsFactors = FALSE)

  # generamos la query
  queries <- base::apply(combinaciones, 1, function(row) {
    terms <- base::paste(row, collapse = " ")
    base::paste0(terms, " in:name,description,readme language:R")
  })

 
  # Ahora se itera entre las variables con las palabras clave (equivalente al AND)
  results <- purrr::map(queries, ~ {
    gh::gh("/search/repositories", q = .x, sort = "stars", order = "desc", per_page = 500)
  })

  return(results)
}