

library(tidyverse)

names(may_absoluta_shapley)
names(dos_tercios_shapley)
names(cuatro_septimos_shapley)
names(tres_quintos_shapley)


may_absoluta_shapley <-  may_absoluta_shapley %>%
  rename(aprob_rech_exitosos_50mas1=aprob_rech_exitosos,
         n_votados=n,
         index_50mas1=index)



dos_tercios_shapley <-  dos_tercios_shapley %>%
  rename(aprob_rech_exitosos_dostercios=aprob_rech_exitosos,
         n_votados_dostercios=n,
         index_dostercios=index)


cuatro_septimos_shapley <-  cuatro_septimos_shapley %>%
  rename(aprob_rech_exitosos_cuatroseptimos=aprob_rech_exitosos,
         n_votados_cuatroseptimos=n,
         index_cuatroseptimos=index)

tres_quintos_shapley <-  tres_quintos_shapley %>%
  rename(aprob_rech_exitosos_tresquintos=aprob_rech_exitosos,
         n_votados_tresquintos=n,
         index_tresquintos=index)


general_shapley <- may_absoluta_shapley %>%
  left_join(dos_tercios_shapley, by="nombre_cc") %>%
  left_join(cuatro_septimos_shapley, by="nombre_cc") %>%
  left_join(tres_quintos_shapley, by="nombre_cc")

