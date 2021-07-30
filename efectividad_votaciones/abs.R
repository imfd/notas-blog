

# Carga de librerías requeridas
library(coda)
library(purrr)
library(dplyr)
library(tidyverse)
library(rvest)
library(tidytext)
library(stringi)
library(stringr)
library(readxl)
library(wnominate)
library(wnomadds)

# Carga objeto con votaciones (Fuente: sala.cconstituyente.cl)
votos_cc <- readRDS("C:/Users/Daniel/Downloads/votos_convencionales_df.rds")

# Carga de votaciones que no están aún en sistema online. Transcripción manual.
votos_cc_first_part <- read_delim("https://storage.googleapis.com/notas-blog-public/nominate/votos_cc_first_part.csv",";", escape_double = FALSE, locale = locale(encoding = "latin1"), trim_ws = TRUE) %>% select(c(nombres_cc,primera_dec_presos_01,segunda_dec_presos_01))

votos_cc_first_part<-votos_cc_first_part %>% pivot_longer(c(primera_dec_presos_01,segunda_dec_presos_01), values_to = "tipo_voto",names_to = "id_votacion") %>%.[,c(1,3,2)]


votos_cc_first_part <- na.omit(votos_cc_first_part)

votos_cc_first_part$nombres_cc <- tolower(votos_cc_first_part$nombres_cc)
votos_cc$nombres_cc <- tolower(votos_cc$nombres_cc)

unique(votos_cc_first_part$nombres_cc)
unique(votos_cc$nombres_cc)


a <- voto

head(votos_cc_first_part)
head(votos_cc)

votos_cc_first_part$tipo_voto[votos_cc_first_part$tipo_voto==1] <- "Aprueba"

votos_cc_first_part$tipo_voto[votos_cc_first_part$tipo_voto==6] <- "Rechaza"

votos_cc_first_part$tipo_voto[votos_cc_first_part$tipo_voto==9] <- "Abstiene"


votos<- rbind(votos_cc_first_part, votos_cc)



votos_convencionales_df <- votos



ps <- votos_convencionales_df %>%
  count(id_votacion, tipo_voto)

ps <- votos_convencionales_df %>%
  count(nombres_cc, tipo_voto)

names(ps)


ps2 <- ps %>%
  pivot_wider(names_from = tipo_voto, values_from = n)


ps2 <- ps2 %>%
  rowwise() %>%
  mutate(total_Votos=sum(Aprueba,Rechaza,Abstiene, na.rm = T))


ps2 <- as.data.frame(ps2)

ps2 <- ps2 %>%
  mutate(prop_afavor=Aprueba/total_Votos,
         prop_encontra=Rechaza/total_Votos,
         prop_abs=Abstiene/total_Votos,
         )

ps2 <- ps2 %>%
  mutate(votos_rechazado_general=47, 
         votos_aprobados_general=64)



#################
####################

# este for debe repetirse para cada resulta y hacer leftjoin tantas veces sea necesario
# debemos añadir un id de las votaciones de presos.
# por ahora no agregaremos 

id_votaciones <- votaciones_df %>% select(Id, Resultado)

id_votaciones_manuales <- data.frame(Id=c("primera_dec_presos_01","segunda_dec_presos_01"),
                                     Resultado=c("RECHAZADO", "APROBADO"))


id_votaciones <- rbind(id_votaciones, id_votaciones_manuales)


id_votaciones_rechazadas <- id_votaciones %>%
  filter(Resultado=='RECHAZADO')


id_votaciones_aprobadas <- id_votaciones %>%
  filter(Resultado=='APROBADO')


nombres_convencionales <- as.character(ps2$nombres_cc)


veces_convencional_all <- data.frame()

# primero votaciones aprueba para aprobados

for (i in 1:155) {
  
  ps_2 <- votos_convencionales_df %>%
    filter(tipo_voto=='Aprueba' & nombres_cc==nombres_convencionales[i])
  
  
  ps_2 <- ps_2 %>%
    filter(id_votacion %in% id_votaciones_aprobadas$Id)
  
  
  veces_convencional <- ps_2 %>% count(nombres_cc) %>% rename(veces_aprueba_to_aprobar=n)
  
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional)
  
  
}



ps2_v2 <- ps2 %>% left_join(veces_convencional_all, by="nombres_cc")


veces_convencional_all <- data.frame()


#  votaciones rechazadas con aprobación

for (i in 1:155) {
  
  ps_2 <- votos_convencionales_df %>%
    filter(tipo_voto=='Rechaza' & nombres_cc==nombres_convencionales[i])
  
  
  ps_2 <- ps_2 %>%
    filter(id_votacion %in% id_votaciones_aprobadas$Id)
  
  
  veces_convencional <- ps_2 %>% count(nombres_cc) %>% rename(veces_rechaza_to_aprobar=n)
  
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional)
  
  
}



ps2_v2 <- ps2_v2 %>% left_join(veces_convencional_all, by="nombres_cc")





veces_convencional_all <- data.frame()


#  votaciones abstienen con aprobación

for (i in 1:155) {
  
  ps_2 <- votos_convencionales_df %>%
    filter(tipo_voto=='Abstiene' & nombres_cc==nombres_convencionales[i])
  
  
  ps_2 <- ps_2 %>%
    filter(id_votacion %in% id_votaciones_aprobadas$Id)
  
  
  veces_convencional <- ps_2 %>% count(nombres_cc) %>% rename(veces_abstiene_to_aprobar=n)
  
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional)
  
  
}



ps2_v2 <- ps2_v2 %>% left_join(veces_convencional_all, by="nombres_cc")





##########################################################
#################################################################
########################################################################


veces_convencional_all <- data.frame()

#  votaciones aprueba con rechazo

for (i in 1:155) {
  
  ps_2 <- votos_convencionales_df %>%
    filter(tipo_voto=='Aprueba' & nombres_cc==nombres_convencionales[i])
  
  
  ps_2 <- ps_2 %>%
    filter(id_votacion %in% id_votaciones_rechazadas$Id)
  
  
  veces_convencional <- ps_2 %>% count(nombres_cc) %>% rename(veces_aprueba_to_rechazar=n)
  
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional)
  
  
}



ps2_v2 <- ps2_v2 %>% left_join(veces_convencional_all, by="nombres_cc")






veces_convencional_all <- data.frame()

#  votaciones rechaza con rechazo

for (i in 1:155) {
  
  ps_2 <- votos_convencionales_df %>%
    filter(tipo_voto=='Rechaza' & nombres_cc==nombres_convencionales[i])
  
  
  ps_2 <- ps_2 %>%
    filter(id_votacion %in% id_votaciones_rechazadas$Id)
  
  
  veces_convencional <- ps_2 %>% count(nombres_cc) %>% rename(veces_rechaza_to_rechazar=n)
  
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional)
  
  
}



ps2_v2 <- ps2_v2 %>% left_join(veces_convencional_all, by="nombres_cc")











veces_convencional_all <- data.frame()

#  votaciones abstiene con rechazo

for (i in 1:155) {
  
  ps_2 <- votos_convencionales_df %>%
    filter(tipo_voto=='Abstiene' & nombres_cc==nombres_convencionales[i])
  
  
  ps_2 <- ps_2 %>%
    filter(id_votacion %in% id_votaciones_rechazadas$Id)
  
  
  veces_convencional <- ps_2 %>% count(nombres_cc) %>% rename(veces_abstiene_to_rechazar=n)
  
  
  veces_convencional_all <- rbind(veces_convencional_all, veces_convencional)
  
  
}



ps2_v2 <- ps2_v2 %>% left_join(veces_convencional_all, by="nombres_cc")





#######################################
########################################
######################################


votos_efectivos <- ps2_v2 %>% 
  select(nombres_cc, Aprueba, Rechaza, Abstiene,veces_abstiene_to_rechazar, veces_rechaza_to_rechazar, veces_aprueba_to_aprobar) %>%
  mutate(votaciones_aprobadas=65, 
         votaciones_rechazadas=48)


votos_cc_first_part <- read_delim("https://storage.googleapis.com/notas-blog-public/nominate/votos_cc_first_part.csv",";", 
                                  escape_double = FALSE, locale = locale(encoding = "latin1"), trim_ws = TRUE) 

votos_coalicion <- votos_cc_first_part %>%
  select(nombres_cc,coalicion )


votos_coalicion$nombres_cc <- tolower(votos_coalicion$nombres_cc)


votos_coalicion_efectividad <- votos_coalicion %>% left_join(votos_efectivos, by="nombres_cc")
votos_coalicion_efectividad <- votos_coalicion_efectividad %>%
mutate(prop_aprobacion=veces_aprueba_to_aprobar/votaciones_aprobadas*100)


votos_coalicion_efectividad <- votos_coalicion_efectividad %>%
  rowwise() %>%
  mutate(veces_suma_abs_rech_to_rechazar=sum(veces_abstiene_to_rechazar, veces_rechaza_to_rechazar, na.rm = TRUE),
         prop_rechazo= veces_suma_abs_rech_to_rechazar/votaciones_rechazadas*100)

votos_coalicion_efectividad <- data.frame(votos_coalicion_efectividad)

# visualizar

p10 <- ggplot(votos_coalicion_efectividad, aes(x = coalicion, y =prop_aprobacion, fill=coalicion)) +scale_fill_viridis_d() + theme_bw()+
  theme(legend.position = 'none')+
  geom_boxplot() + scale_y_continuous(limits=c(0,100)) + ggtitle("% de efectividad de votar A favor para aprobar")
p10





p11 <- ggplot(votos_coalicion_efectividad, aes(x = coalicion, y =prop_rechazo, fill=coalicion)) + scale_fill_viridis_d() + theme_bw()+
  geom_boxplot() + scale_y_continuous(limits=c(0,100)) + ggtitle("% de efectividad de votar En Contra+Abstencion para rechazar")

p11



pp <- gridExtra::grid.arrange(p10, p11, ncol=2)


ggsave(plot=pp, "plot2.png", width = 21, height = 11)


# a mayor abstención, menor aprobacion, se usará para reforzar el rechazo sin mancharse? 




apruebas <- ps %>%
  filter(tipo_voto=='Aprueba') %>%
  arrange(desc(n)) %>% top_n(10) %>%
    ggplot(aes(reorder(nombres_cc, n), n, size=5)) + ylab("Frecuencia") + xlab("Constituyente") +
  geom_point(stat = "identity") + coord_flip() + theme_minimal() + theme(legend.position = 'none') +
  ggtitle("A Favor")
apruebas

rechazo <- ps %>%
  filter(tipo_voto=='Rechaza') %>%
  arrange(desc(n)) %>% top_n(10) %>%
  ggplot(aes(reorder(nombres_cc, n), n, size=5)) + ylab("Frecuencia") + xlab("Constituyente") +
  geom_point(stat = "identity") + coord_flip() + theme_minimal() + theme(legend.position = 'none') +
  ggtitle("En Contra")
rechazo

abstencion <- ps %>%
  filter(tipo_voto=='Abstiene') %>%
  arrange(desc(n)) %>% top_n(10) %>%
  ggplot(aes(reorder(nombres_cc, n), n, size=5)) + ylab("Frecuencia") + xlab("Constituyente") +
  geom_point(stat = "identity") + coord_flip() + theme_minimal() + theme(legend.position = 'none') +
  ggtitle("Abstención")
abstencion


p <- gridExtra::grid.arrange(apruebas, rechazo, abstencion, ncol=3)


