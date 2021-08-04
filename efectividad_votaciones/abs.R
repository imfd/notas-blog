

# Carga de librerías requeridas
library(purrr)
library(tidyverse)
library(rvest)
library(tidytext)
library(stringi)
library(stringr)
library(readxl)

# Carga objeto con votaciones (Fuente: sala.cconstituyente.cl)
votos_cc <- read_rds("input/votos_convencionales_df.rds")
votaciones_df <- read_rds("input/votaciones_df.rds")
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

votos_cc_first_part <- read_excel("input/agrupamientos.xlsx")

names(votos_cc_first_part)

votos_coalicion <- votos_cc_first_part %>%
  select(nombres_cc,cupo2, coalicion_actual  )


votos_coalicion$nombres_cc <- tolower(votos_coalicion$nombres_cc)


votos_coalicion_efectividad <- votos_coalicion %>% left_join(votos_efectivos, by="nombres_cc")
votos_coalicion_efectividad <- votos_coalicion_efectividad %>%
mutate(prop_aprobacion=veces_aprueba_to_aprobar/votaciones_aprobadas*100)


votos_coalicion_efectividad <- votos_coalicion_efectividad %>%
  rowwise() %>%
  mutate(veces_suma_abs_rech_to_rechazar=sum(veces_abstiene_to_rechazar, veces_rechaza_to_rechazar, na.rm = TRUE),
         prop_rechazo= veces_suma_abs_rech_to_rechazar/votaciones_rechazadas*100)

votos_coalicion_efectividad <- data.frame(votos_coalicion_efectividad)


votos_coalicion_efectividad <- votos_coalicion_efectividad %>%
  rowwise() %>%
  mutate(suma_aprueba_to_aprueba_rechaza_to_rechaza=sum(veces_aprueba_to_aprobar,veces_rechaza_to_rechazar, na.rm = TRUE))
votos_coalicion_efectividad <- as.data.frame(votos_coalicion_efectividad)

votos_coalicion_efectividad <- votos_coalicion_efectividad %>%
  mutate(perct_efectividad=suma_aprueba_to_aprueba_rechaza_to_rechaza/113*100)


# visualizar

unique(votos_coalicion_efectividad$cupo2)

votos_coalicion_efectividad$cupo2[votos_coalicion_efectividad$cupo2=='Resto de lista']<- "Lis. Apruebo(resto)"
votos_coalicion_efectividad$cupo2[votos_coalicion_efectividad$cupo2=='Movimientos Soc. Const.']<- "MovimientosSoc.Const."

library(forcats)

votos_coalicion_efectividad$nombres_cc <- str_to_title(votos_coalicion_efectividad$nombres_cc)
votos_coalicion_efectividad$perct_efectividad <- round(votos_coalicion_efectividad$perct_efectividad,2)

efectividad_general <- ggplot(votos_coalicion_efectividad %>%
                                mutate(Grupo=fct_reorder(cupo2, perct_efectividad, .fun = median, .desc = TRUE)) %>%
                                rename(Efectividad=perct_efectividad, Nombre=nombres_cc), 
                              aes(x = Grupo,
                                  y = Efectividad, fill=Grupo)) +geom_boxplot()+ #geom_point(aes(label=Nombre)) +
  scale_y_continuous(limits=c(0,100)) +ggtitle("% de efectividad de votaciones")+ xlab("Alianzas") + ylab("%")+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=9))+
  scale_fill_manual(values = c("steelblue","steelblue","steelblue","steelblue","steelblue",
                               "steelblue","steelblue","steelblue", "steelblue","steelblue")) + theme_bw()+ 
  theme(legend.position = 'none',axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20), plot.title = element_text(size=25))

efectividad_general



ggsave(plot=efectividad_general,"output/efectividad_general.png", width=27, height = 15)



############# tabla resumen

tabla_show <- data.frame()

names <- as.character(unique(votos_coalicion_efectividad$cupo2))

for (i in 1:10) {
  ldp <- votos_coalicion_efectividad %>%
    filter(cupo2==names[i])
  
  
  df <- data.frame(unclass(summary(ldp$perct_efectividad)), check.names = FALSE, stringsAsFactors = FALSE) %>% 
    rownames_to_column("measure") 
  
  df <- df %>% rename(value=`unclass(summary(ldp$perct_efectividad))`) %>%
    pivot_wider(names_from = measure, values_from = value) %>% mutate(cupo=names[i]) %>%
    select(cupo, everything())
  
  
  tabla_show <- rbind(tabla_show, df)
  
  
  
}

tabla_show[,-1] <-round(tabla_show[,-1],2)

tabla_show


writexl::write_xlsx(tabla_show,"output/tabla.xlsx")


# tabla interactiva

votos_coalicion_efectividad_tt<- votos_coalicion_efectividad %>%
  rename(Constituyente=nombres_cc,Alianza=cupo2, A_Favor=Aprueba, En_Contra=Rechaza,
       Abstenciones=Abstiene,votos_efectivos_A_Favor=veces_aprueba_to_aprobar, 
       votos_efectivos_En_Contra= veces_rechaza_to_rechazar,Votos_Efectivos_General=suma_aprueba_to_aprueba_rechaza_to_rechaza,
       Efectividad=perct_efectividad)

votos_coalicion_efectividad_tt <- votos_coalicion_efectividad_tt %>%
  select(Constituyente, Alianza, A_Favor, En_Contra,Abstenciones,votos_efectivos_A_Favor,
         votos_efectivos_En_Contra, Votos_Efectivos_General, Efectividad)

votos_coalicion_efectividad_tt$Efectividad <- round(votos_coalicion_efectividad_tt$Efectividad, 2)


votos_coalicion_efectividad_tt$Efectividad<- paste0(votos_coalicion_efectividad_tt$Efectividad,"%")


library(DT)
votos_coalicion_efectividad_tt <- votos_coalicion_efectividad_tt %>% arrange(desc(Efectividad))

votos_coalicion_efectividad_tt$Constituyente <- str_to_title(votos_coalicion_efectividad_tt$Constituyente)


p <- DT::datatable(votos_coalicion_efectividad_tt)

p



saveWidget(p, file="index.html")
