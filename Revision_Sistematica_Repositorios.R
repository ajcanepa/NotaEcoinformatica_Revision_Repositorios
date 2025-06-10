# Carga de Paquetes -------------------------------------------------------
library("tidyverse")
library("cranlogs")
library("patchwork")
library("BiocPkgTools")
library("igraph")
library("ggraph")
library("tidygraph")
library("gh")
library("purrr")
library("scales")
library("viridis")


# Consultando CRAN --------------------------------------------------------
# Cargamos el listado de paquetes desde CRAN
Cran <- tools::CRAN_package_db() %>% tibble::as_tibble()

# Filtramos según la búsqueda/query usada
Cran_filtrado <- 
  Cran %>%
  mutate(
    text = paste(Package, Title, Description, sep = " "),
    keep = grepl("alignment|sequence alignment|multiple alignment", 
                 text, ignore.case = TRUE) &
      grepl("pipeline|workflow|nextflow|reproducible workflow|workflow automation", 
            text, ignore.case = TRUE)
  ) %>%
  filter(keep) %>%
  select(-text, -keep)


# Evolución temporal
# Convertir fechas y extraer el año
df_years <- Cran_filtrado %>%
  mutate(pub_date = as.Date(`Date/Publication`),
         pub_year = year(pub_date)) %>%
  count(pub_year)

# Crear secuencia de todos los años en el rango
all_years <- data.frame(pub_year = seq(min(df_years$pub_year, na.rm = TRUE),
                                       max(df_years$pub_year, na.rm = TRUE))) 

# Unir para asegurar que todos los años estén presentes
df_years_complete <- all_years %>%
  left_join(df_years, by = "pub_year") %>%
  mutate(n = ifelse(is.na(n), 0, n))

# Graficar
Paquetes_anio <- 
  ggplot(df_years_complete, aes(x = as.factor(pub_year), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Paquetes publicados por año",
       x = "",
       y = "Número de paquetes") +
  geom_text(aes(label = n), vjust = -0.5, fontface = "bold", size = 2.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.7, vjust = 1.5))

Paquetes_anio


# Paquetes más descargados
# Obtener el vector de nombres de paquetes
Package_names <- Cran_filtrado %>% pull(Package)

# Obtener el número de descargas de los últimos 7 días
Cran_filtrado_downloads_lastweek <- 
  cran_downloads(when = "last-week", package = Package_names) %>% 
  tibble::as_tibble()

Paquetes_descarga <- 
  Cran_filtrado_downloads_lastweek %>%
  group_by(package) %>%
  summarise(total_downloads = sum(count), .groups = "drop") %>% 
  #filter(total_downloads > 300) %>% 
  ggplot(., aes(x = reorder(package, -total_downloads), y = total_downloads)) +
  geom_col(fill = "steelblue") +
  labs(title = "Descarga de los paquetes desde el CRAN (última semana)",
       x = "",
       y = "Número de descargas") +
  geom_text(aes(label = total_downloads), vjust = -0.5, fontface = "bold", size = 2.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2))

Paquetes_descarga

# Gráfica combinada
Grafico_combinado <-
  Paquetes_anio + Paquetes_descarga + plot_layout(ncol = , 2, widths = c(1, 1.5))

Grafico_combinado


# Consultando Bioconductor ------------------------------------------------
# Cargamos el listado de paquetes desde Bioconductor
Bioconductor <- BiocPkgTools::biocPkgList()

# Filtramos según la búsqueda/query usada
Bioconductor_filtrado <- 
  Bioconductor %>%
  mutate(
    text = paste(Package, Title, Description, sep = " "),
    keep = grepl("alignment|sequence alignment|multiple alignment", 
                 text, ignore.case = TRUE) &
      grepl("pipeline|workflow|nextflow|reproducible workflow|workflow automation", 
            text, ignore.case = TRUE)
  ) %>%
  filter(keep) %>%
  select(-text, -keep)

Bioconductor_filtrado 

# Gráfico de dependencias del paquete `CircSeqAlignTk` 
# comenzamos con el objeto que contiene todos los paquetes de Bioconductor
Bioconductor

# Encontramos los paquetes que dependen de `CircSeqAlignTk`
Dep_pkgs <- Bioconductor %>%
  filter(Package == "CircSeqAlignTk") %>%
  pull("Imports") %>%
  unlist()

# Creamos la lista del borde del gráfico
edges <- data.frame(
  from = Dep_pkgs,
  to = rep("CircSeqAlignTk", length(Dep_pkgs)),
  stringsAsFactors = FALSE
)

# Creamos el objeto `igraph`
graph <- graph_from_data_frame(edges, directed = TRUE)

# Graficamos con ggraph
Grafico_dependencias <- 
ggraph(graph, layout = "circle") +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')), end_cap = circle(3, 'mm')) +
  geom_node_label(aes(label = name), repel = TRUE) +
  theme_void() +
  ggtitle("Paquetes que dependen de CircSeqAlignTk")

Grafico_dependencias

# Consultando GitHub ------------------------------------------------------
# Establecemos el "Personal Access Token" 
Sys.setenv(GITHUB_TOKEN = "your_token_here")

# El query equivalente se ha de realizar combinando los strings del OR y uniéndolos luego (quivalencia con el AND)
keywords_group_1 <- c("alignment", "\"sequence alignment\"", "\"multiple alignment\"")
keywords_group_2 <- c("pipeline", "workflow", "nextflow", "\"reproducible workflow\"", "\"workflow automation\"")

# generamos la query
queries <- expand.grid(keywords_group_1, keywords_group_2, stringsAsFactors = FALSE) %>%
  transmute(query = paste0(Var1, " ", Var2, " in:name,description,readme language:R"))


# Ahora se itera entre las variables con las palabras clave (equivalente al AND)
results <- map(queries$query, ~ {
  gh::gh("/search/repositories", q = .x, sort = "stars", order = "desc", per_page = 500)
})

# Almacenamos la listas de los resultados en un objeto de tipo data.frame
repo_df <- 
  results %>%
  map("items") %>%
  flatten() %>%
  unique() %>%
  map_df(~ {
    tibble::tibble(
      name = .x$name,
      full_name = .x$full_name,
      description = .x$description,
      url = .x$html_url,
      stars = .x$stargazers_count,
      language = .x$language,
      license = safely(~ .x$license$name)(.x)$result,  # Safely extract license
      owner = safely(~ .x$owner$login)(.x)$result,      # Safely extract owner login
      created_at = .x$created_at,
      updated_at = .x$updated_at,
      pushed_at = .x$pushed_at,
      forks = .x$forks_count,
      open_issues = .x$open_issues_count,
      size = .x$size,
      watchers = .x$watchers_count,
      has_issues = .x$has_issues,
      has_wiki = .x$has_wiki,
      has_pages = .x$has_pages
    )
  })

repo_df

# Para graficar variación en el tiempo
# convertimos los atributos de tiempo y calculamos intervalos
repo_df <-
  repo_df %>% 
  mutate(
    created_at = as.Date(created_at),
    updated_at = as.Date(updated_at),
    pushed_at = as.Date(pushed_at),
    age_years = interval(created_at, Sys.Date()) / years(1),
    score = stars + forks + watchers - open_issues
  )

# Variacion temporal de las estrellas
Variacion_estrellas <- 
repo_df %>%
  filter(stars > 25) %>%
  ggplot(., aes(x = created_at, y = reorder(name, created_at))) +
  geom_point(aes(color = stars), size = 3) +
  scale_color_viridis(option = "C", trans = "log10", na.value = "gray") +
  labs(
    title = "Creación de repositorios",
    subtitle = "Línea temporal",
    x = "Fecha creación",
    y = "Nombre del repositorio",
    color = "Estrellas \n (log10)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

Variacion_estrellas
