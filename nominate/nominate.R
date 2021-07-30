
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
library(pscl)
library(wnominate)
library(wnomadds)

# Carga objetos con votaciones
votos_cc <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  bind_rows()


# Agrega id de votación por fila 
votos_cc$id_votacion <- paste0("bol_",votos_cc$id_votacion)

<<<<<<< HEAD

votos_cc <- votos_cc %>%
  pivot_wider(names_from = id_votacion, values_from = tipo_voto)

#votos_cc <- read.csv("https://raw.githubusercontent.com/tresquintos/tresquintos.github.io/master/datos/convenci%C3%B3n/cc%20rollcall.csv")


=======
# Transforma a columnas IDs de votación
votos_cc <- votos_cc %>%
  pivot_wider(names_from = id_votacion, values_from = tipo_voto)

# Recodificación votación
>>>>>>> a0bff38ac342815596ddb3f0b9843e5511f3df1e
votos_cc <- as.data.frame(votos_cc)
votos_cc[votos_cc=="NULL"] = 'NA'
votos_cc[votos_cc=="A Favor"] <- "1"
votos_cc[votos_cc=='c("A Favor", "Abstencion")'] <- '1'
votos_cc[votos_cc=="En Contra"] <- "6"
votos_cc[votos_cc=="Abstencion"] <- "9"



# Carga de votaciones que no están aún en sistema online. Transcripción manual.
votos_cc_first_part <- read_delim("https://storage.googleapis.com/notas-blog-public/nominate/votos_cc_first_part.csv", 
                                  ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                  trim_ws = TRUE)

# Join dfs
votos_cc <- votos_cc %>% 
  left_join(votos_cc_first_part %>% select(-c(glosa_cand)),by="nombres_cc")

<<<<<<< HEAD
votos_cc <- votos_cc[,-c(2:36)]
##


=======

# Asignación nombre filas
>>>>>>> a0bff38ac342815596ddb3f0b9843e5511f3df1e
votos_cc <-  votos_cc %>%
  column_to_rownames("nombres_cc")


parlamentarios <- data.frame(parlamentario=rownames(votos_cc))



legData_party_bol <- votos_cc %>%
  select(coalicion) %>%
  rename(party=coalicion)




votos_cc <- votos_cc  %>%
  select(-c(glosa_part, coalicion))


cols.num <- names(votos_cc)
votos_cc[cols.num] <- sapply(votos_cc[cols.num],as.numeric)


rc_party_bol <- rollcall(data = votos_cc, 
                         legis.names = parlamentarios$parlamentario,
                         desc = "Votaciones CC",
                         legis.data = legData_party_bol,
                         yea = 1, nay = 6, missing = 9, notInLegis = NA)

<<<<<<< HEAD
rc_party_bol


wmoni_party_bol <- wnominate(rc_party_bol, polarity = c(106,66))

ideal_1 <- wnominate(rc_party_bol, polarity = c(106,55)) # ideal point
ideal_1

summary(ideal_1)

=======
>>>>>>> a0bff38ac342815596ddb3f0b9843e5511f3df1e

##########################
# W-Nominate - Plots
##########################

# 1D usando wmominate #Omite dos casos (Squella y Harboe)

wmoni_party_bol <- wnominate(rc_party_bol, polarity = 106,dims=1,trials = 200)

# Reconstruccion usando datos paquete wnominate

df_graph <- data.frame(name=rownames(wmoni_party_bol$legislators),coalicion=wmoni_party_bol$legislators$coalicion,coord=wmoni_party_bol$legislators$coord1D,se=wmoni_party_bol$legislators$se1D)

# Crea funciones mean y sd y las agrega como columnas

mean_valid<-function(x){round(mean(na.omit(x)),3)}
sd_valid<-function(x){round(sd(x,na.rm=TRUE),3)}
df_graph$mean_party<-ave(df_graph$coord,df_graph$coalicion,FUN=mean_valid)
df_graph$sd_party<-ave(df_graph$coord,df_graph$coalicion,FUN=sd_valid)

# Crea columna resumen

for (i in 1:length(df_graph$name)){df_graph$gral[i]<-paste0(df_graph$coalicion[i],'\n Prom:',df_graph$mean[i],' (',df_graph$sd[i],')')}

# Plot W-Nominate (individual) 1 Dimensión

p<-df_graph %>%
  ggplot(aes(reorder(name,coord), coord, label=name)) +
  xlab("Nombre") + ylab("Puntaje unidimensional")+
  geom_point(stat='identity', aes(col=coalicion), size=2.5) + scale_color_viridis_d() + coord_flip()+theme_bw()+ theme(legend.position="top")+theme(legend.position = c(0.8, 0.4))+geom_errorbar(aes(ymin=coord-se, ymax=coord+se), width=.2,position=position_dodge(0.05))
p

<<<<<<< HEAD
ggsave(plot = p, "ideal_point_lec.png", width = 22, height = 25)


##########################
##########################
# 2D


wmoni_party_bol <- wnominate(rc_party_bol, polarity = c(106,66))


wmoni_party_bol

plot.coords(wmoni_party_bol)

coords <- wmoni_party_bol$legislators
plot.coords(wmoni_party_bol, legend.x = 1, legend.y = 1, plotBy = "party",
            main.title = "W-NOMINATE", d1.title = "Primera Dimensi?n",
            d2.title = "Segunda Dimensi?n")

=======
# Plot W-Nominate por coalicion 1 Dimensión
>>>>>>> a0bff38ac342815596ddb3f0b9843e5511f3df1e

p <-df_graph%>%
  ggplot(aes(reorder(gral,mean_party), coord, label=name)) +
  xlab("Pacto") + ylab("Puntaje unidimensional")+
  geom_point(stat='identity', aes(col=coalicion), size=2,alpha = 0.4,show.legend = FALSE) + scale_color_viridis_d() + coord_flip()+ theme_bw()
p


# Plot W-Nominate 2 Dimensiones

wmoni_party_bol_2d <- wnominate(rc_party_bol, polarity = c(106,66),trials=200)
wmoni_party_bol_2d
plot.coords(wmoni_party_bol_2d)
plot.coords(wmoni_party_bol_2d, legend.x = 1, legend.y = 1, plotBy = "coalicion",
            main.title = "W-NOMINATE", d1.title = "Primera Dimensión",
            d2.title = "Segunda Dimensión")
