# Análisis DW-Nominate a votaciones CC.

-   Este repositorio puede ser encontrado en <https://github.com/imfd/Wnominate_CC>.

-   Se realizó un *web scraping* de las votaciones realizadas en la CC, contenidas en la web [Sala Constituyente](https://sala.cconstituyente.cl) a través de la librería [*rvest*](cran.r-project.org/web/packages/rvest) de R en el software Rstudio. Al momento de redacción del análisis existe un grupo de votaciones (correspondientes a la primera sesión y referidas a elección de mesa directiva y declaración sobre presos) que no habían sido subidas al sistema. Se codificaron manualmente para ser procesadas. La base de datos con las votaciones fue cargada al archivo "votos_cc.rds" disponible en [Google Cloud platform](https://storage.googleapis.com/notas-blog-public/nominate/votos_cc.rds), Se descartó a priori a aquellas votaciones que usaban un formato de votación distinto del clásico legislativo (ej. votación papal).

-   Además, para el análisis se codificó data sobre los convencionales constituyentes. Ésta se encuentra disponible en el archivo "legData_party_bol.rds", disponible en [Google Cloud Platform](https://storage.googleapis.com/notas-blog-public/nominate/legData_party_bol.rds).

-   Se usó la librería de R [*pscl*](https://cran.r-project.org/web/packages/pscl/index.html) para la construcción de rollcalls y [*dwnominate*](https://github.com/wmay/dwnominate) *para la estimación del dw-Nominate.*

-   Nominate fija requisitos para la utilización de cada votación y de cada constituyente (o congresista, en su uso tradicional). Se utilizaron los parámetros por defecto, correspondientes a un mínimo de 20 votos por individuo y un mínimo de 2,5% en la opción minoritaria para votaciones desequilibradas (lopsided votes).

-   El número total de votaciones consideradas hasta la sesión ordinaria número catorce (10 de agosto 2021) es 118. Este repositorio aumentará progresivamente la data usada para análisis en la medida que la cantidad de votaciones disponibles aumente.
