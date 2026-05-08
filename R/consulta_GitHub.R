#Función para consulta en GitHub
consulta_GitHub <- function(query=NULL) {

  #Comprobamos que los paquetes necesarios están instalados, sino, se instalan 
  paquetes <- c("gh", "purrr", "dplyr", "tibble")
  for (paquete in paquetes){
    if(!require(paquete, character.only = TRUE)){
      utils::install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }

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
  # El query equivalente se ha de realizar combinando los strings del OR y uniéndolos luego (equivalencia con el AND)
  combinaciones <- base::expand.grid(condiciones_OR, stringsAsFactors = FALSE)

  # generamos la query
  queries <- base::apply(combinaciones, 1, function(row) {
    terms <- base::paste(row, collapse = " ")
    base::paste(terms, "in:name,description,readme", "language:R")

  })

  # Ahora se itera entre las variables con las palabras clave (equivalente al AND)
  results <- purrr::map(queries, function(x) {
    gh::gh("/search/repositories", q = x, sort = "stars", order = "desc", per_page = 500)
  })

# Almacenamos la listas de los resultados en un objeto de tipo data.frame
repo_df <- 
  results %>%
  map("items") %>%
  flatten() %>%
  unique() %>%
  map_df(function(x) {
    tibble::tibble(
      name = x$name,
      full_name = x$full_name,
      description = x$description,
      url = x$html_url,
      stars = x$stargazers_count,
      language = x$language,
      license = safely(~ x$license$name)(x)$result,  # Extracción de licencia de forma segura
      owner = safely(~ x$owner$login)(x)$result,      # Extracción del inicio de sesión del propiertario de forma segura
      created_at = x$created_at,
      updated_at = x$updated_at,
      pushed_at = x$pushed_at,
      forks = x$forks_count,
      open_issues = x$open_issues_count,
      size = x$size,
      watchers = x$watchers_count,
      has_issues = x$has_issues,
      has_wiki = x$has_wiki,
      has_pages = x$has_pages
    )
  })
return(repo_df)
  
}