



library(jsonlite)
library(tidyverse)
library(rvest)
library(httr)
library(tidytext)
library(stringi)
library(stringr)
library(readxl)

sesiones_json<- fromJSON("http://sala.cconstituyente.cl/doGet.asmx/getSesiones")
sesiones_df<- data.frame(numero=sesiones_json[["data"]][["Numero"]],id=sesiones_json[["data"]][["Id"]],fechatexto=sesiones_json[["data"]][["FechaTexto"]])
b<-vector()
for(i in 1:length(sesiones_df$id)){sesiones_df$n_votaciones[i]<-eval(parse(text=paste0('NROW(fromJSON("http://sala.cconstituyente.cl/doGet.asmx/getVotacionesPorSesion?prmSesionId=',sesiones_df$id[i],'")[["data"]])')))}

votaciones<-list()
for(i in 1:length(sesiones_df$id)){votaciones[[i]]<-eval(parse(text=paste0('fromJSON("http://sala.cconstituyente.cl/doGet.asmx/getVotacionesPorSesion?prmSesionId=',sesiones_df$id[i],'")[["data"]]')))}
votaciones_df<-Reduce(bind_rows, votaciones)
votaciones_df$url<-paste0("https://sala.cconstituyente.cl/views/VotacionDetalle.aspx?prmVotacionId=",votaciones_df$Id)










# aqui extraigo las votaciones de la sesion, el número parece no ser correlativo y va a ir creciendo cada día más
# se debe automatizar eso
df_votaciones_sesion <- fromJSON("http://sala.cconstituyente.cl/doGet.asmx/getVotacionesPorSesion?prmSesionId=4062")

df_votaciones_sesion <- df_votaciones_sesion[["data"]]


# este nombre de sesion yo se lo daba en base a la sesion que estabamos bajando

id_sesion<- 8


# esta url debe ser pegada con el id de cada votacion
url_madre <- "https://sala.cconstituyente.cl/views/VotacionDetalle.aspx?prmVotacionId="


# obtengo todos los ids
ids <- unique(df_votaciones_sesion$Id)

# el largo de ids que tenemos en esta sesion
extension <- length(ids)


# dejamos un df vacío
df_all_votes <- data.frame()

for (i in 1:extension) {
  
  url_votacion <- paste0(url_madre,ids[i]) # se pega url_madre y el ids
  
  id <- stri_extract_last_regex(url_votacion, "\\d{3}") # extraigo el id de la votacion para posterior join
  
  # a favor 
  
  afavor <- url_votacion %>%
    html_session() %>%
    html_nodes('#form1 > div.row.row-flex > div > div:nth-child(2)') %>% # como el html que se forma es básico, es facil identificar por ´posición
    html_text() %>% data.frame()
  
  afavor$txt <- afavor$.
  
  afavor$txt <- as.character(afavor$txt)
  
  afavor2 <- afavor %>%
    unnest_tokens(chapter, txt, token = "regex", pattern = "\r\n ")
  
  afavor2 <- afavor2 %>%
    distinct()
  
  afavor2$chapter <- str_trim(afavor2$chapter)
  
  
  
  afavor2  <- afavor2 %>%
    select(chapter) %>%
    rename(nombres_cc=chapter)
  afavor2$tipo_voto <- "A Favor"
  
  afavor2$id_votacion <- id
  
  afavor2 <- afavor2 %>%
    filter(nombres_cc!="")
  
  
  
  # en contra 
  enContra <- url_votacion %>%
    html_session() %>%
    html_nodes('#form1 > div.row.row-flex > div > div:nth-child(4)') %>%
    html_text() %>% data.frame()
  
  enContra
  
  
  
  enContra$txt <- enContra$.
  
  enContra$txt <- as.character(enContra$txt)
  
  enContra <- enContra %>%
    unnest_tokens(chapter, txt, token = "regex", pattern = "\r\n")
  
  enContra <- enContra %>%
    distinct()
  
  enContra$chapter <- str_trim(enContra$chapter)
  
  
  enContra  <- enContra %>%
    select(chapter) %>%
    rename(nombres_cc=chapter)
  enContra$tipo_voto <- "En Contra"
  
  enContra$id_votacion <- id
  
  enContra <- enContra %>%
    filter(nombres_cc!="")
  
  
  
  
  # abstencion
  
  abstencion <- url_votacion %>%
    html_session() %>%
    html_nodes('#form1 > div.row.row-flex > div > div:nth-child(6)') %>%
    html_text() %>% data.frame()
  
  abstencion
  
  
  
  abstencion$txt <- abstencion$.
  
  abstencion$txt <- as.character(abstencion$txt)
  
  abstencion <- abstencion %>%
    unnest_tokens(chapter, txt, token = "regex", pattern = "\r\n")
  
  abstencion <- abstencion %>%
    distinct()
  
  abstencion$chapter <- str_trim(abstencion$chapter)
  
  
  abstencion  <- abstencion %>%
    select(chapter) %>%
    rename(nombres_cc=chapter)
  abstencion$tipo_voto <- "Abstencion"
  abstencion$id_votacion <- id
  
  
  
  abstencion <- abstencion %>%
    filter(nombres_cc!="")
  
  
  df_votacion <- rbind(afavor2, enContra, abstencion)
  
  
  df_all_votes <- rbind(df_all_votes, df_votacion)
  
 # saveRDS(df_all_votes,paste0("temp/votos_sesion_",id_sesion,".rds")) # por ahora las dejaremos sueltas
  
  saveRDS(df_all_votes,paste0("votos_sesion_",id_sesion,".rds")) # se guardaran las votaciones de cada sesion 
  
  
  
  
}

