


library(tidyverse)
library(readr)


# mayoria absoluta
names(rechazos_mayoria_absoluta)


aprobaciones_mayoria_absoluta <- read_rds("temp/aprobaciones_mayoria_absoluta.rds") %>%
  rename(aprobaciones_exitosas_mayoria_absoluta=aprobaciones_exitosas,
         n_afavor_mayoria_absoluta=n_afavor,
         index_shapley_aprobaciones_mayoria_absoluta=index,
         n_iniciativas_aprobadas_mayoria_absoluta=n_iniciativas_aprobadas,
         prop_efectividad_aprobacion_individual_mayoria_absoluta=prop_efectividad_individual,
         prop_efectividad_sobre_proyectos_aprobados_mayoria_abosluta=prop_efectividad_sobre_proyectos_aprobados
         )
rechazos_mayoria_absoluta <- read_rds("temp/rechazos_mayoria_absoluta.rds")%>%
  rename(rechazos_abs_exitosas_mayoria_absoluta=rechazos_existosos,
         n_rechazos_abs_mayoria_absoluta=n_encontra,
         index_shapley_rechazos_mayoria_absoluta=index,
         n_iniciativas_rechazadas_mayoria_absoluta=n_iniciativas_rechazadas,
         prop_efectividad_rechazar_abs_individual_mayoria_absoluta=prop_efectividad_individual,
         prop_efectividad_sobre_proyectos_rechazados_mayoria_abosluta=prop_efectividad_sobre_proyectos_rechazados)




votaciones_mayoria_absoluta <- aprobaciones_mayoria_absoluta %>%
  left_join(rechazos_mayoria_absoluta, by="nombre_cc")
votaciones_mayoria_absoluta <- votaciones_mayoria_absoluta %>%
  rowwise() %>%
  mutate(votaciones_exitosas_mayoria_absoluta=sum(aprobaciones_exitosas_mayoria_absoluta,rechazos_abs_exitosas_mayoria_absoluta),
        votaciones_individual_mayoria_absoluta=sum(n_afavor_mayoria_absoluta, n_rechazos_abs_mayoria_absoluta),
        n_iniciativas_votadas_mayoria_absoluta=sum(n_iniciativas_aprobadas_mayoria_absoluta, n_iniciativas_rechazadas_mayoria_absoluta),
        prop_efectividad_general_individual_mayor_absoluta=votaciones_exitosas_mayoria_absoluta/votaciones_individual_mayoria_absoluta,
        prop_efectividad_general_sobre_total_iniciativas_mayoria_absoluta=votaciones_exitosas_mayoria_absoluta/n_iniciativas_votadas_mayoria_absoluta) %>%
  as_data_frame()



# dos tercios

aprobaciones_dos_tercios <- read_rds("temp/aprobaciones_dos_tercios.rds") %>%
  rename(aprobaciones_exitosas_dos_tercios=aprobaciones_exitosas,
         n_afavor_dos_tercios=n_afavor,
         index_shapley_aprobaciones_dos_tercios=index,
         n_iniciativas_aprobadas_dos_tercios=n_iniciativas_aprobadas,
         prop_efectividad_aprobacion_individual_dos_tercios=prop_efectividad_individual,
         prop_efectividad_sobre_proyectos_aprobados_dos_tercios=prop_efectividad_sobre_proyectos_aprobados
  )
rechazos_dos_tercios <- read_rds("temp/rechazos_dos_tercios.rds") %>%
  rename(rechazos_abs_exitosas_dos_tercios=rechazos_existosos,
         n_rechazos_abs_dos_tercios=n_encontra,
         index_shapley_rechazos_dos_tercios=index,
         n_iniciativas_rechazadas_dos_tercios=n_iniciativas_rechazadas,
         prop_efectividad_rechazar_abs_individual_dos_tercios=prop_efectividad_individual_rechazos,
         prop_efectividad_sobre_proyectos_rechazados_dos_tercios=prop_efectividad_sobre_proyectos_rechazados)


votaciones_dos_tercios<- aprobaciones_dos_tercios %>%
  left_join(rechazos_dos_tercios, by="nombre_cc")

votaciones_dos_tercios <- votaciones_dos_tercios %>%
  rowwise() %>%
  mutate(votaciones_exitosas_dos_tercios=sum(aprobaciones_exitosas_dos_tercios,rechazos_abs_exitosas_dos_tercios),
         votaciones_individual_dos_tercios=sum(n_afavor_dos_tercios, n_rechazos_abs_dos_tercios),
         n_iniciativas_votadas_dos_tercios=sum(n_iniciativas_aprobadas_dos_tercios, n_iniciativas_rechazadas_dos_tercios),
         prop_efectividad_general_individual_dos_tercios=votaciones_exitosas_dos_tercios/votaciones_individual_dos_tercios,
         prop_efectividad_general_sobre_total_iniciativas_dos_tercios=votaciones_exitosas_dos_tercios/n_iniciativas_votadas_dos_tercios) %>%
  as_data_frame()


# tresquintos


aprobaciones_tres_quintos <- read_rds("temp/aprobaciones_tres_quintos.rds") %>%
  rename(aprobaciones_exitosas_tres_quintos=aprobaciones_exitosas,
         n_afavor_tres_quintos=n_afavor,
         index_shapley_aprobaciones_tres_quintos=index,
         n_iniciativas_aprobadas_tres_quintos=n_iniciativas_aprobadas,
         prop_efectividad_aprobacion_individual_tres_quintos=prop_efectividad_individual,
         prop_efectividad_sobre_proyectos_aprobados_tres_quintos=prop_efectividad_sobre_proyectos_aprobados
  )
rechazos_tres_quintos <- read_rds("temp/rechazos_tres_quintos.rds") %>%
  rename(rechazos_abs_exitosas_tres_quintos=rechazos_existosos,
         n_rechazos_abs_tres_quintos=n_encontra,
         index_shapley_rechazos_tres_quintos=index,
         n_iniciativas_rechazadas_tres_quintos=n_iniciativas_rechazadas,
         prop_efectividad_rechazar_abs_individual_tres_quintos=prop_efectividad_individual_rechazos,
         prop_efectividad_sobre_proyectos_rechazados_tres_quintos=prop_efectividad_sobre_proyectos_rechazados)


votaciones_tres_quintos<- aprobaciones_tres_quintos %>%
  left_join(rechazos_tres_quintos, by="nombre_cc")

votaciones_tres_quintos<- votaciones_tres_quintos %>%
  rowwise() %>%
  mutate(votaciones_exitosas_tres_quintos=sum(aprobaciones_exitosas_tres_quintos,rechazos_abs_exitosas_tres_quintos),
         votaciones_individual_tres_quintos=sum(n_afavor_tres_quintos, n_rechazos_abs_tres_quintos),
         n_iniciativas_votadas_tres_quintos=sum(n_iniciativas_aprobadas_tres_quintos, n_iniciativas_rechazadas_tres_quintos),
         prop_efectividad_general_individual_tres_quintos=votaciones_exitosas_tres_quintos/votaciones_individual_tres_quintos,
         prop_efectividad_general_sobre_total_iniciativas_tres_quintos=votaciones_exitosas_tres_quintos/n_iniciativas_votadas_tres_quintos) %>%
  as_data_frame()




# cuatro septimos


aprobaciones_cuatro_septimos <- read_rds("temp/aprobaciones_cuatro_septimos.rds") %>%
  rename(aprobaciones_exitosas_cuatro_septimos=aprobaciones_exitosas,
         n_afavor_cuatro_septimos=n_afavor,
         index_shapley_aprobaciones_cuatro_septimos=index,
         n_iniciativas_aprobadas_cuatro_septimos=n_iniciativas_aprobadas,
         prop_efectividad_aprobacion_individual_cuatro_septimos=prop_efectividad_individual,
         prop_efectividad_sobre_proyectos_aprobados_cuatro_septimos=prop_efectividad_sobre_proyectos_aprobados
  )
rechazos_cuatro_septimos<- read_rds("temp/rechazos_cuatro_septimos.rds") %>%
  rename(rechazos_abs_exitosas_cuatro_septimos=rechazos_existosos,
         n_rechazos_abs_cuatro_septimos=n_encontra,
         index_shapley_rechazos_cuatro_septimos=index,
         n_iniciativas_rechazadas_cuatro_septimos=n_iniciativas_rechazadas,
         prop_efectividad_rechazar_abs_individual_cuatro_septimos=prop_efectividad_individual_rechazos,
         prop_efectividad_sobre_proyectos_rechazados_cuatro_septimos=prop_efectividad_sobre_proyectos_rechazados)






votaciones_cuatro_septimos<- aprobaciones_cuatro_septimos %>%
  left_join(rechazos_cuatro_septimos, by="nombre_cc")

votaciones_cuatro_septimos <- votaciones_cuatro_septimos %>%
  rowwise() %>%
  mutate(votaciones_exitosas_cuatro_septimos=sum(aprobaciones_exitosas_cuatro_septimos,rechazos_abs_exitosas_cuatro_septimos),
         votaciones_individual_cuatro_septimos=sum(n_afavor_cuatro_septimos, n_rechazos_abs_cuatro_septimos),
         n_iniciativas_votadas_cuatro_septimos=sum(n_iniciativas_aprobadas_cuatro_septimos, n_iniciativas_rechazadas_cuatro_septimos),
         prop_efectividad_general_individual_cuatro_septimos=votaciones_exitosas_cuatro_septimos/votaciones_individual_cuatro_septimos,
         prop_efectividad_general_sobre_total_iniciativas_cuatro_septimos=votaciones_exitosas_cuatro_septimos/n_iniciativas_votadas_cuatro_septimos) %>%
  as_data_frame()




# juntaremos toda la efectividad

# 


efectividad_general <- votaciones_mayoria_absoluta %>%
  left_join(votaciones_dos_tercios,by="nombre_cc") %>%
  left_join(votaciones_cuatro_septimos, by="nombre_cc") %>%
  left_join(votaciones_tres_quintos, by="nombre_cc")


# remover variables repetidad o que duplican informacion
names(efectividad_general)
efectividad_general <- efectividad_general %>%
  select(-c(n_afavor_dos_tercios,n_afavor_tres_quintos, n_afavor_cuatro_septimos,n_rechazos_abs_tres_quintos, n_rechazos_abs_cuatro_septimos, n_rechazos_abs_dos_tercios)) %>%
  rename(n_votaciones_afavor=n_afavor_mayoria_absoluta,
        n_votaciones_encontra =n_rechazos_abs_mayoria_absoluta)



efectividad_general <- efectividad_general %>%
  select(-c(votaciones_individual_cuatro_septimos, votaciones_individual_dos_tercios,votaciones_individual_tres_quintos)) %>%
  rename(n_iniciativas_votadas_total=votaciones_individual_mayoria_absoluta)
efectividad_general <- as.data.frame(efectividad_general)
efectividad_general <- efectividad_general %>%
  select(nombre_cc,aprobaciones_exitosas_mayoria_absoluta, aprobaciones_exitosas_dos_tercios, aprobaciones_exitosas_cuatro_septimos,
         aprobaciones_exitosas_tres_quintos,rechazos_abs_exitosas_tres_quintos,votaciones_exitosas_tres_quintos,
         rechazos_abs_exitosas_tres_quintos,votaciones_exitosas_tres_quintos,n_iniciativas_votadas_tres_quintos,
         n_iniciativas_aprobadas_tres_quintos,n_iniciativas_rechazadas_tres_quintos,prop_efectividad_aprobacion_individual_tres_quintos,
         prop_efectividad_rechazar_abs_individual_tres_quintos,prop_efectividad_general_individual_tres_quintos,
         prop_efectividad_sobre_proyectos_aprobados_tres_quintos, prop_efectividad_sobre_proyectos_rechazados_tres_quintos,
         index_shapley_aprobaciones_tres_quintos, index_shapley_rechazos_tres_quintos,rechazos_abs_exitosas_mayoria_absoluta, rechazos_abs_exitosas_dos_tercios, rechazos_abs_exitosas_cuatro_septimos,
         votaciones_exitosas_mayoria_absoluta, votaciones_exitosas_dos_tercios, votaciones_exitosas_cuatro_septimos,
         n_votaciones_afavor, n_votaciones_encontra, n_iniciativas_votadas_total, n_iniciativas_aprobadas_mayoria_absoluta,
         n_iniciativas_rechazadas_mayoria_absoluta,n_iniciativas_aprobadas_dos_tercios, n_iniciativas_rechazadas_dos_tercios,
         n_iniciativas_aprobadas_cuatro_septimos, n_iniciativas_rechazadas_cuatro_septimos,prop_efectividad_aprobacion_individual_mayoria_absoluta,
         prop_efectividad_aprobacion_individual_dos_tercios,prop_efectividad_aprobacion_individual_cuatro_septimos, prop_efectividad_rechazar_abs_individual_mayoria_absoluta,
         prop_efectividad_rechazar_abs_individual_dos_tercios, prop_efectividad_rechazar_abs_individual_cuatro_septimos, prop_efectividad_general_individual_mayor_absoluta,
         prop_efectividad_general_individual_dos_tercios, prop_efectividad_general_individual_cuatro_septimos,
         prop_efectividad_sobre_proyectos_aprobados_mayoria_abosluta,
         prop_efectividad_sobre_proyectos_aprobados_dos_tercios, prop_efectividad_sobre_proyectos_aprobados_cuatro_septimos,
         prop_efectividad_sobre_proyectos_rechazados_mayoria_abosluta, prop_efectividad_sobre_proyectos_rechazados_dos_tercios,
         prop_efectividad_sobre_proyectos_rechazados_cuatro_septimos,
         index_shapley_aprobaciones_mayoria_absoluta, index_shapley_aprobaciones_dos_tercios, index_shapley_aprobaciones_cuatro_septimos,
         index_shapley_rechazos_mayoria_absoluta, index_shapley_rechazos_dos_tercios, index_shapley_rechazos_cuatro_septimos
         )

general_shapley2 <- general_shapley %>%
  left_join(efectividad_general, by="nombre_cc")


general_shapley2 <- general_shapley2 %>%
  left_join(leg_data_party, by=c("nombre_cc"="nombres_cc")) %>%
  select(nombre_cc, coalicion, grupo_actual,cupo, cupo_recod,genero, distrito, everything())

general_shapley2_df <- general_shapley2

writexl::write_xlsx(general_shapley2, "shapley_efectividad_162_v2.xlsx")

names(general_shapley2_df)
general_shapley2_df <- general_shapley2_df %>%
  mutate(normalized_index_50mas1=normalize(index_50mas1),
         normalized_index_dostercios=normalize(index_dostercios),
         normalized_index_cuatroseptimos=normalize(index_cuatroseptimos),
         normalized_index_tresquinto=normalize(index_tresquintos),
         normalized_index_shapley_aprobaciones_mayoria_absoluta=normalize(index_shapley_aprobaciones_mayoria_absoluta),
         normalized_index_shapley_aprobaciones_dos_tercios=normalize(index_shapley_aprobaciones_dos_tercios),
         normalized_index_shapley_aprobaciones_cuatro_septimos=normalize(index_shapley_aprobaciones_cuatro_septimos),
         normalized_index_shapley_aprobaciones_tresquintos=normalize(index_shapley_aprobaciones_tres_quintos),
         normalized_index_shapley_rechazos_mayoria_absoluta=normalize(index_shapley_rechazos_mayoria_absoluta),
         normalized_index_shapley_rechazos_tresquintos=normalize(index_shapley_rechazos_tres_quintos),
         normalized_index_shapley_rechazos_dos_tercios=normalize(index_shapley_rechazos_dos_tercios),
         normalized_index_shapley_rechazos_cuatro_septimos=normalize(index_shapley_rechazos_cuatro_septimos))


sum(general_shapley2_df$normalized_index_50mas1)
sum(general_shapley2_df$normalized_index_tresquinto)



