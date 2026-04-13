#Función para consulta en Bioconductor
consulta_Bioconductor <- function(query=NULL) {

  # Cargamos el listado de paquetes desde Bioconductor
  Bioconductor <- BiocPkgTools::biocPkgList()

  #Excepción para caso de llamada a función sin query
  if (base::is.null(query) || !base::is.character(query) || base::trimws(query) == "") {
    stop("Necesitas entregar un query a la función para realizar el filtrado")
  }

  # Convertir la query al formato Bioconductor si no contiene ya | o &
  if(!base::grepl("\\||&", query)){
      query <- base::gsub("\\s+or\\s+", "|", query, ignore.case= TRUE)
      query <- base::gsub("\\s+and\\s+", "&", query, ignore.case= TRUE)
  }
  
  # Dividimos la query en las distintas condiciones unidas por AND (funciona con número indefinido de AND)
  condiciones_AND <- base::strsplit(query, "&")[[1]]
  condiciones_AND <- base::trimws(condiciones_AND)

  # Filtramos según la búsqueda/query usada
  Bioconductor_filtrado <- 
  Bioconductor %>%
  dplyr::mutate(
      text = base::paste(Package, Title, Description, sep = " "),
      keep = base::Reduce(
        `&`,
        base::lapply(condiciones_AND, function(g) {
          base::grepl(g, text, ignore.case = TRUE)
        })
      )
  ) %>%
  dplyr::filter(keep) %>%
  dplyr::select(-text, -keep)
}