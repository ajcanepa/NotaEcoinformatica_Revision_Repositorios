# Revisión sistemática de repositorios para el lenguaje R (CRAN, GitHub, Bioconductor)
por: [Antonio Jesús Canepa Oneto](https://github.com/ajcanepa) y [Claudia Fernández Ruiz](https://github.com/cfr1012)


### Introducción
En este repositorio se muestra el código de la nota ecoinformática homónima, disponible aquí [Actualizar codigo.]

### Objetivos
El principal objetivo es proveer de una guía de exploración o consulta sistemática de los repositorios más comunes del lenguaje de programación R, como son [CRAN](https://cran.r-project.org/), [Bioconductor](https://www.bioconductor.org/) y [GitHub](https://github.com/).

Este trabajo presenta tres funciones interoperables (`consulta_CRAN()`, `consulta_Bioconductor()` y `consulta_GitHub()`) que permiten realizar **búsquedas sistemáticas y reproducibles** mediante un motor de consulta booleano común a los repositorios CRAN, Bioconductor y GitHub. Superando así la heterogeneidad sintáctica propia de cada plataforma y devolviendo, en todos los casos, estructuras de datos homogéneas y directamente comparables; permitiendo crear visualizaciones como:

* Listado de paquetes creados y descargados en CRAN

<img src="Figuras/CRAN_Publicados_Descargados.png" alt="Listado de paquetes creados y descargados en CRAN" width="75%">


***

* Gráfico de dependencias de paquetes en Bioconductor

<img src="Figuras/Bioconductor_Dependencias.png" alt="Gráfico de dependencias de paquetes en Bioconductor" width="75%">

***

* Fecha de creación y número de estrellas de paquetes en GitHub

<img src="Figuras/Github_estrellas.png" alt="Gráfico de estrellas en GitHub" width="75%">

***

Esta aproximación constituye una herramienta metodológica de utilidad tanto para quienes desarrollan nuevos paquetes de R como para quienes emprenden revisiones sistemáticas del software científico disponible.

***