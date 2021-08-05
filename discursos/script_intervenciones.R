


library(tidyverse)
library(cowplot)

library(patchwork)
library(readr)
intervencionsYEAH <- read_csv("input/intervencionsYEAH.csv")
intervencionsYEAH <- intervencionsYEAH %>% distinct() %>% janitor::clean_names()

library(readxl)


agrupamientos <- read_excel("input/agrupamientos.xlsx") %>% select(nombres_cc, coalicion, cupo, cupo2) %>%
  mutate(nombres_cc=tolower(nombres_cc))

library(lubridate)
res <- intervencionsYEAH %>%
  mutate(seconds_duration=hms(duracion), 
         seconds_duration=hour(seconds_duration)*60 + minute(seconds_duration))      # format to 'hours:minutes:seconds'

res$orador <- tolower(res$orador)
table(res$seconds_duration)


plot(res$seconds_duration)


res <- res %>% left_join(agrupamientos, by=c("orador"="nombres_cc"))

res_count_pactos <- res %>% count(cupo2)


### add img



res_count_pactos$cupo2[res_count_pactos$cupo2=='Resto de lista']<- "Lis. Apruebo(resto)"
res_count_pactos$cupo2[res_count_pactos$cupo2=='Movimientos Soc. Const.']<- "MovimientosSoc.Const."
res_count_pactos$cupo2[res_count_pactos$cupo2=='Independientes']<- "Independientes(resto)"

plot_count_alianza <- res_count_pactos %>% arrange(desc(n))%>% ggplot(aes(reorder(cupo2,desc(-n)),n, fill=cupo2)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("coral1","coral1","coral1","coral1","coral1",
                               "coral1","coral1","coral1","coral1","coral1")) + labs(title = "Discursos e intervenciones por alianzas en la Convención Constitucional",
                                                                                     x="Alianza-Grupo coordinado",
                                                                                     y="N° de discursos")   + geom_text(aes(label=n),fontface = "bold", position=position_dodge(width=0.9), hjust=-0.25) +
  theme(legend.position = 'none') +coord_flip() + theme_bw()

plot_count_alianza



plot_count_alianza <- plot_count_alianza + theme(plot.margin = unit(c(1, 1, 3, 1), "lines"),legend.position='none',axis.text.x=element_text(hjust=0, face = "bold", family = "bold"),
                                                 axis.text.y=element_text(hjust=0, vjust=1, face = "bold",family = "bold"),
                                                 axis.title.x =element_text(hjust=0.5,face = "bold", family = "bold"),
                                                 axis.title.y = element_text(hjust=0.5,face = "bold", family = "bold")) 
plot_count_alianza <- plot_count_alianza + theme(plot.title=element_text(size=14, face="bold", colour = "black"))+ scale_y_continuous(limits = c(0,130))

logo <- system.file('input/telar_logo.png', package = 'patchwork')
logo <- png::readPNG("input/telar_logo.png", native = TRUE)

# adding image to graph 
img_graph <- plot_count_alianza +                  
  inset_element(p = logo,
                0.88, 0.088, 0.99,0.197)


img_graph 


ggsave(plot = img_graph,"output/plot_discursos.png", width = 9, height = 7 )


by_cc_n_interventions <- res %>%
  count(orador)

by_cc_n_interventions_times <- res %>%
  group_by(orador) %>%
  summarise(time_discursos=sum(seconds_duration))


by_cc_n_interventions_times2 <- by_cc_n_interventions_times %>% left_join(agrupamientos, by=c("orador"="nombres_cc"))



by_alianzas_duracion_discursos <- by_cc_n_interventions_times2 %>%
  group_by(cupo2) %>%
  summarise(time_discursos=sum(time_discursos))

by_count_intervenciones <- by_cc_n_interventions_times2 %>% count(cupo2)

by_alianzas_duracion_discursos$periods <- seconds_to_period(by_alianzas_duracion_discursos$time_discursos)


by_alianzas_duracion_discursos$cupo2[by_alianzas_duracion_discursos$cupo2=='Resto de lista']<- "Lis. Apruebo(resto)"
by_alianzas_duracion_discursos$cupo2[by_alianzas_duracion_discursos$cupo2=='Movimientos Soc. Const.']<- "MovimientosSoc.Const."
by_alianzas_duracion_discursos$cupo2[by_alianzas_duracion_discursos$cupo2=='Independientes']<- "Independientes(resto)"




plot_count_alianza <- by_alianzas_duracion_discursos %>% arrange(desc(time_discursos))%>% ggplot(aes(reorder(cupo2,desc(-time_discursos)),time_discursos, fill=cupo2)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c("coral1","coral1","coral1","coral1","coral1",
                               "coral1","coral1","coral1","coral1","coral1")) + labs(title = "Tiempo de discursos e intervenciones por alianzas en la Convención Constitucional",
                                                                                     x="Alianza-Grupo coordinado",
                                                                                     y="Segundos")   + geom_text(aes(label=periods),fontface = "bold", position=position_dodge(width=0.9), hjust=-0.25) +
  theme(legend.position = 'none') +coord_flip() + theme_bw()

plot_count_alianza



plot_count_alianza <- plot_count_alianza + theme(plot.margin = unit(c(1, 1, 3, 1), "lines"),legend.position='none',axis.text.x=element_text(hjust=0, face = "bold", family = "bold"),
                                                 axis.text.y=element_text(hjust=0, vjust=1, face = "bold",family = "bold"),
                                                 axis.title.x =element_text(hjust=0.5,face = "bold", family = "bold"),
                                                 axis.title.y = element_text(hjust=0.5,face = "bold", family = "bold")) 
plot_count_alianza <- plot_count_alianza + theme(plot.title=element_text(size=14, face="bold", colour = "black"))+ scale_y_continuous(limits = c(0,20000))

logo <- system.file('input/telar_logo.png', package = 'patchwork')
logo <- png::readPNG("input/telar_logo.png", native = TRUE)

# adding image to graph 
img_graph <- plot_count_alianza +                  
  inset_element(p = logo,
                0.88, 0.088, 0.99,0.197)


img_graph 


ggsave(plot = img_graph,"output/plot_discursos_times.png", width = 10, height = 7 )



