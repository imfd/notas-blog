

# Carga de librerías requeridas
library(purrr)
library(tidyverse)
library(rvest)
library(tidytext)
library(stringi)
library(stringr)
library(readxl)
library(ggplot2)



# descargamos votaciones

leg_data_party <- read_rds("input/legData_party_bol3.rds")
votos_cc <- read_rds("input/votos_cc3.rds")


votos_cc <- as.data.frame(votos_cc)  %>% rownames_to_column("nombre_cc") %>%
  pivot_longer(!nombre_cc,names_to = "votacion", values_to="voto")

info_votaciones <-read_rds("input/votaciones_df.rds")


nombres_convencionales <- leg_data_party$nombres_cc



votos_cc$votacion <- gsub("bol_","", votos_cc$votacion)

votos_cc$voto[votos_cc$voto==1] <- "Aprueba"

votos_cc$voto[votos_cc$voto==6] <- "Rechaza"

votos_cc$voto[votos_cc$voto==9] <- "Abstiene"



unique(votos_cc$votacion)


votos_cc <- votos_cc %>%
  filter(votacion!='primera_dec_presos_01')
votos_cc <- votos_cc %>%
  filter(votacion!='segunda_dec_presos_01')




votos_cc <- votos_cc %>%
  filter(votacion %in% unique(info_votaciones$Id))

info_votaciones <- info_votaciones %>%
  filter(Id %in% unique(votos_cc$votacion))








info_votaciones_aprobadas <- info_votaciones %>%
  filter(Resultado=='APROBADO' & TotalSI>=103)


info_votaciones_rechazadas_dos_tercios<- info_votaciones %>%
  filter(Resultado=='APROBADO' & TotalSI<103)

info_votaciones_rechazadas <- info_votaciones %>%
  filter(Resultado=='RECHAZADO')


info_votaciones_rechazadas <- rbind(info_votaciones_rechazadas, info_votaciones_rechazadas_dos_tercios)

veces_convencional_all <- data.frame()

# primero votaciones aprueba para aprobados con dos tercios

for (i in 1:155) {
  
  ps_2 <- votos_cc %>%
    filter(voto=="Aprueba" & nombre_cc==nombres_convencionales[i])
  
  aprobaciones <- ps_2 %>% count(nombre_cc) %>% rename(n_afavor=n)
  
  ps_2 <- ps_2 %>%
    filter(votacion %in% info_votaciones_aprobadas$Id)
  
  total_afavor <- info_votaciones_aprobadas %>%
    filter(Id %in% ps_2$votacion) %>% select(Id,TotalSI)
  
  total_afavor$Id <- as.character(total_afavor$Id)
  
  total_afavor <- ps_2 %>%
    left_join(total_afavor, by=c("votacion"="Id"))
  
  
  
  number_deno <- aprobaciones$n_afavor
  
  ###
  
  total_afavor <- total_afavor %>%
    mutate(pre_index=1/TotalSI,
           index=(sum(pre_index))/number_deno)
  
  
  aprobaciones_exitosas <- ps_2 %>% count(nombre_cc) %>% rename(aprobaciones_exitosas=n)  
  
  
  veces_convencional_all_aux <- aprobaciones_exitosas%>%
    left_join(aprobaciones, by="nombre_cc") %>%
    left_join(total_afavor %>% select(nombre_cc,index)%>% distinct(),by="nombre_cc")
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional_all_aux)
  
  rm(veces_convencional_all_aux)
  
  
  
}






veces_convencional_all$n_iniciativas_aprobadas <- 60


veces_convencional_all$prop_efectividad_individual <- veces_convencional_all$aprobaciones_exitosas/veces_convencional_all$n_afavor
veces_convencional_all$prop_efectividad_sobre_proyectos_aprobados <- veces_convencional_all$aprobaciones_exitosas/veces_convencional_all$n_iniciativas_aprobadas





veces_convencional_all_rechazos <- data.frame()

# primero votaciones rechaza  para rechazados

for (i in 1:155) {
  
  ps_2 <- votos_cc %>%
    filter( nombre_cc==nombres_convencionales[i]) %>%
    filter(voto=="Rechaza" | voto=='Abstiene')
  
  rechazos <- ps_2 %>% count(nombre_cc) %>% rename(n_encontra=n)
  
  
  
  ps_2 <- ps_2 %>%
    filter(votacion %in% info_votaciones_rechazadas$Id)
  
  
  total_encontra_abs <- info_votaciones_rechazadas %>%
    filter(Id %in% ps_2$votacion) %>% select(Id,TotalNO)
  
  total_encontra_abs$Id <- as.character(total_encontra_abs$Id)
  
  total_encontra_abs <- ps_2 %>%
    left_join(total_encontra_abs, by=c("votacion"="Id"))
  
  
  
  number_deno <- rechazos$n_encontra
  
  ###
  
  total_encontra_abs <- total_encontra_abs %>%
    mutate(pre_index=1/TotalNO,
           index=(sum(pre_index))/number_deno)
  
  
  rechazos_existosos <- ps_2 %>% count(nombre_cc) %>% 
    rename(rechazos_existosos=n)  
  
  veces_convencional_all_aux2 <- rechazos_existosos%>%
    left_join(rechazos, by="nombre_cc") %>%
    left_join(total_encontra_abs %>% select(nombre_cc,index)%>% distinct(),by="nombre_cc")
  
  
  
  
  veces_convencional_all_rechazos <- rbind(veces_convencional_all_rechazos, veces_convencional_all_aux2)
  
  rm(veces_convencional_all_aux2)
  
}



veces_convencional_all_rechazos$n_iniciativas_rechazadas<- 102

veces_convencional_all_rechazos$prop_efectividad_individual_rechazos<- veces_convencional_all_rechazos$rechazos_existosos/veces_convencional_all_rechazos$n_encontra
veces_convencional_all_rechazos$prop_efectividad_sobre_proyectos_rechazados <- veces_convencional_all_rechazos$rechazos_existosos/veces_convencional_all_rechazos$n_iniciativas_rechazadas


# renombrar dos tercios

aprobaciones_dos_tercios <- veces_convencional_all

rechazos_dos_tercios <- veces_convencional_all_rechazos



write_rds(aprobaciones_dos_tercios,"temp/aprobaciones_dos_tercios.rds")

write_rds(rechazos_dos_tercios,"temp/rechazos_dos_tercios.rds")





