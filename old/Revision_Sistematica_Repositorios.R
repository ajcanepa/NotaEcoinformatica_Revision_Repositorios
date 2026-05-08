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
#Cargamos la función para unificar las búsquedas
source('R/consulta_CRAN.R')

# Filtramos según la búsqueda/query usada
Cran_filtrado <- consulta_CRAN(query = "alignment or sequence alignment or multiple alignment and pipeline or workflow or nextflow or reproducible workflow or workflow automation")

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
  labs(title = "Descarga de paquetes desde CRAN",
       subtitle = "Última semana",
       x = "",
       y = "Número de descargas") +
  geom_text(aes(label = total_downloads), vjust = -0.5, fontface = "bold", size = 2.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2))

# Gráfica combinada
Grafico_combinado <-
  Paquetes_anio + Paquetes_descarga + plot_layout(ncol = , 2, widths = c(1, 1.5))

# Descarga de gráfico (opcional)
#ggplot2::ggsave(filename = "CRAN_Publicados_Descargados.png", plot = Grafico_combinado, path = paste(getwd(), "/Figuras", sep = ""), scale = 1.2, width = 25, height = 15, units = "cm", dpi = 150)


# Consultando Bioconductor ------------------------------------------------
#Cargamos la función para unificar las búsquedas
source('R/consulta_Bioconductor.R')

# Filtramos según la búsqueda/query usada
Bioconductor_filtrado <- consulta_Bioconductor(query = "alignment or sequence alignment or multiple alignment and pipeline or workflow or nextflow or reproducible workflow or workflow automation")

# Gráfico de dependencias del paquete `CircSeqAlignTk` 
# comenzamos con el objeto que contiene todos los paquetes de Bioconductor
Bioconductor <- BiocPkgTools::biocPkgList()

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

# Descarga de gráfico (opcional)
#ggplot2::ggsave(filename = "Bioconductor_Dependencias.png", plot = Grafico_dependencias, path = paste(getwd(), "/Figuras", sep = ""), scale = 1.2, width = 25, height = 25, units = "cm", dpi = 150, bg = "white")


# Consultando GitHub ------------------------------------------------------
#Cargamos la función para unificar las búsquedas
source('R/consulta_GitHub.R')

# Establecemos el "Personal Access Token" 
Sys.setenv(GITHUB_TOKEN = "tu_clave_de_acceso")

# Filtramos sobre GitHub usando una query con operadores booleanos
GitHub_filtrado <- consulta_GitHub(query = "alignment or sequence alignment or multiple alignment and pipeline or workflow or nextflow or reproducible workflow or workflow automation")

# Para graficar variación en el tiempo
# convertimos los atributos de tiempo y calculamos intervalos
repo_df <-
  GitHub_filtrado %>% 
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

# Descarga de gráfico (opcional)
#ggplot2::ggsave(filename = "Github_estrellas.png", plot = Variacion_estrellas, path = paste(getwd(), "/Figuras", sep = ""), scale = 1.1, width = 15, height = 25, units = "cm", dpi = 150, bg = "white")


#Guardar los gráficos en el archivo
save.image(file = "Revision_Sistematica_Repositorios.RData")
