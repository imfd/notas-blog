


library(tidyverse)
library(rvest)
library(tidytext)
library(stringi)
library(stringr)
library(readxl)
library(pscl)
library(wnominate)
library(wnomadds)


votos_cc <- list.files(pattern = ".rds") %>%
  map(readRDS) %>% 
  bind_rows()



votos_cc$id_votacion <- paste0("bol_",votos_cc$id_votacion)


votos_cc <- votos_cc %>%
  pivot_wider(names_from = id_votacion, values_from = tipo_voto)

#votos_cc <- read.csv("https://raw.githubusercontent.com/tresquintos/tresquintos.github.io/master/datos/convenci%C3%B3n/cc%20rollcall.csv")


votos_cc <- as.data.frame(votos_cc)
votos_cc[votos_cc=="NULL"] = 'NA'
votos_cc[votos_cc=="A Favor"] <- "1"
votos_cc[votos_cc=='c("A Favor", "Abstencion")'] <- '1'
votos_cc[votos_cc=="En Contra"] <- "6"
votos_cc[votos_cc=="Abstencion"] <- "9"



# aqui antes transcribimos las votaciones de declaracion sobre los presos y todas las votaciones de eleccion de la mesa de la CC
votos_cc_first_part <- read_delim("https://storage.googleapis.com/notas-blog-public/nominate/votos_cc_first_part.csv", 
                                  ";", escape_double = FALSE, locale = locale(encoding = "latin1"), 
                                  trim_ws = TRUE)
names(votos_cc_first_part)

votos_cc <- votos_cc %>% 
  left_join(votos_cc_first_part %>% select(-c(glosa_cand)),by="nombres_cc")

votos_cc <- votos_cc[,-c(2:36)]
##


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

rc_party_bol


wmoni_party_bol <- wnominate(rc_party_bol, polarity = c(106,66))

ideal_1 <- wnominate(rc_party_bol, polarity = c(106,55)) # ideal point
ideal_1

summary(ideal_1)



plot(ideal_1)



pp <- data.frame(ideal_1$xbar)



legData_party_bol <- legData_party_bol %>%
  rownames_to_column(var="name_voter")

pp <- pp %>%
  rownames_to_column(var="name_voter")

pp <- pp %>% 
  left_join(legData_party_bol, by="name_voter")

p <-pp %>%
  ggplot(aes(reorder(name_voter,D1), D1, label=name_voter)) +
  geom_point(stat='identity', aes(col=coalicion), size=2.5) + scale_color_viridis_d() + coord_flip()
p

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





