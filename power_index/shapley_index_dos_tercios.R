






# Carga de librerías requeridas
library(purrr)
library(tidyverse)
library(rvest)
library(tidytext)
library(stringi)
library(stringr)
library(readxl)
library(ggplot2)

leg_data_party <- read_rds("input/legData_party_bol3.rds")
votos_cc <- read_rds("input/votos_cc3.rds")



votos_cc <- as.data.frame(votos_cc) %>% rownames_to_column("nombre_cc") %>%
  pivot_longer(!nombre_cc,names_to = "votacion", values_to="voto")


info_votaciones <-read_rds("input/votaciones_df.rds")


nombres_convencionales <- leg_data_party$nombres_cc


votos_cc <- votos_cc %>%
  filter(votacion!='primera_dec_presos_01')
votos_cc <- votos_cc %>%
  filter(votacion!='segunda_dec_presos_01')



votos_cc$votacion <- gsub("bol_","", votos_cc$votacion)

votos_cc$voto[votos_cc$voto==1] <- "Aprueba"

votos_cc$voto[votos_cc$voto==6] <- "Rechaza"

votos_cc$voto[votos_cc$voto==9] <- "Abstiene"



unique(votos_cc$votacion)



votos_cc <- votos_cc %>%
  filter(votacion %in% unique(info_votaciones$Id))

info_votaciones <- info_votaciones %>%
  filter(Id %in% unique(votos_cc$votacion))

# efectividad en dos tercios

info_votaciones_aprobadas<- info_votaciones %>%
  filter(Resultado=='APROBADO' & TotalSI>=103)


info_votaciones_rechazadas <- info_votaciones %>%
  filter(Resultado=='APROBADO' & TotalSI<103)

info_votaciones_rechazadas2<- info_votaciones %>%
  filter(Resultado=='RECHAZADO')


info_votaciones_rechazadas <- rbind(info_votaciones_rechazadas, info_votaciones_rechazadas2)


veces_convencional_all <- data.frame()

# primero votaciones aprueba para aprobados con dos tercios

for (i in 1:155) {
  
  
  # aprobaciones
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
  
  
  # rechazos
  
  ps_2r <- votos_cc %>%
    filter( nombre_cc==nombres_convencionales[i]) %>%
    filter(voto=="Rechaza" | voto=='Abstiene')
  
  rechazos <- ps_2r %>% count(nombre_cc) %>% rename(n_encontra=n)
  
  
  total_encontra_abs <- info_votaciones_rechazadas %>%
    filter(Id %in% ps_2r$votacion) %>% select(Id,TotalNO)
  
  total_encontra_abs$Id <- as.character(total_encontra_abs$Id)
  
  total_encontra_abs <- ps_2r %>%
    left_join(total_encontra_abs, by=c("votacion"="Id"))
  
  
  ####
  
  total <- rbind(total_afavor %>% rename(total_votos=TotalSI),
                 total_encontra_abs %>% rename(total_votos=TotalNO))  
  total <- na.omit(total)
  
  
  number_deno <- aprobaciones$n_afavor+rechazos$n_encontra
  
  ###
  
  total <- total %>%
    mutate(pre_index=1/total_votos,
           index=(sum(pre_index))/number_deno)
  
  
  aprob_rechazos <- rbind(aprobaciones %>% rename(n=n_afavor), rechazos %>% rename(n=n_encontra)) %>%
    group_by(nombre_cc) %>%
    summarise(n=sum(n))
  
  aprob_rechazos_exitosos <- total %>% count(nombre_cc) %>% rename(aprob_rech_exitosos=n)  
  
  
  ###
  
  
  veces_convencional_all_aux <- aprob_rechazos_exitosos %>%
    left_join(aprob_rechazos, by="nombre_cc") %>%
    left_join(total %>% select(nombre_cc,index)%>% distinct(),by="nombre_cc")
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional_all_aux)
  
  rm(veces_convencional_all_aux)
  
  
  
  
  
  
}
dos_tercios_shapley <- veces_convencional_all
