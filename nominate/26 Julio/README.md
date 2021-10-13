# Análisis W-Nominate a votación CC.

-   Este repositorio puede ser encontrado en <https://github.com/imfd/notas-blog/tree/main/nominate>.

-   Se realizó un *web scrapping* de las votaciones realizadas en la CC en R (Rstudio), contenidas en la web [Sala Constituyente](https://sala.cconstituyente.cl) a través de la librería *rvest*.

-   La construcción del análisis se llevó a cabo en R (con RStudio), y se usaron de forma principal las librerías *pscl* y *wnominate.*

-   Al momento de redacción del análisis existe un grupo de votaciones (las primeras, referidas a la elección de la mesa directiva y declaración sobre presos) que no habían sido subidas al sistema. Se codificaron manualmente, siendo cargadas a [Google Cloud platform](https://storage.googleapis.com/notas-blog-public/nominate/votos_cc_first_part.csv).

-   Además de las mencionadas anteriormente, se utilizaron las votaciones correspondientes a las sesiones 5, 7 y 8. Estas pueden ser encontradas en el repositorio en archivos de formato *.rds*.

-   Se realiza un *join* de ambos datasets para el análisis.

-   A medida que aumente la cantidad de votaciones disponibles aumentaremos también los datos usados como input para el análisis.

-   La técnica W-Nominate fija requisitos para la utilización de cada votación y de cada constituyente (o congresista, en su uso tradicional). Se utilizaron los parámetros por defecto, correspondientes a un mínimo de 20 votos por individuo y un mínimo de 2,5% en la opción minoritaria para votaciones desequilibradas (lopsided votes).

-   Bajo los supuestos anteriores dos convencionales constituyentes fueron excluidos del análisis: Agustín Squella y Felipe Harboe. Además, 32 de 67 votaciones se excluyeron por no satisfacer los requisitos, construyendo el análisis sobre un total de 35 votaciones.

-   Para el cálculo de error estándar se realizaron 200 *trials* en ambas estimaciones (unidimensional y bidimensional).
