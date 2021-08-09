
#  se debe repetir este proceso por cada sesion, pronto lo automatizaremos




library(tidyverse)
library(cowplot)
library(patchwork)
library(readr)
intervencionsYEAH <- read_csv("input/intervencionsYEAH.csv")
intervencionsYEAH <- intervencionsYEAH %>% distinct() %>% janitor::clean_names()

library(readxl)


agrupamientos <- read_excel("input/agrupamientos.xlsx") %>% select(nombres_cc,coalicion, cupo, cupo2) %>%
  mutate(nombres_cc=tolower(nombres_cc))


agrupamientos$cupo2[agrupamientos$cupo2=='Resto de lista']<- "Lis. Apruebo(resto)"
agrupamientos$cupo2[agrupamientos$cupo2=='Movimientos Soc. Const.']<- "MovimientosSoc.Const."
agrupamientos$cupo2[agrupamientos$cupo2=='Independientes']<- "Independientes(resto)"


library(lubridate)
res <- intervencionsYEAH %>%
  mutate(seconds_duration=hms(duracion), 
         seconds_duration=hour(seconds_duration)*60 + minute(seconds_duration))      # format to 'hours:minutes:seconds'
unique(res$sesion)

res <- res %>% filter(sesion==3 | sesion==4 | sesion==5 | sesion==6 |
                        sesion==8 | sesion==9 | sesion==10 | sesion==11 |
                        sesion==12 | sesion==13)


res$orador <- tolower(res$orador)
table(res$seconds_duration)


plot(res$seconds_duration)


res <- res %>% left_join(agrupamientos, by=c("orador"="nombres_cc"))

res_count_pactos <- res %>% count(cupo2)



### add img



res_count_pactos$cupo2[res_count_pactos$cupo2=='Resto de lista']<- "Lis. Apruebo(resto)"
res_count_pactos$cupo2[res_count_pactos$cupo2=='Movimientos Soc. Const.']<- "MovimientosSoc.Const."
res_count_pactos$cupo2[res_count_pactos$cupo2=='Independientes']<- "Independientes(resto)"



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



by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>%
  left_join(res_count_pactos,by="cupo2")


by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>%
  mutate(tiempo_promedio=time_discursos/n)

by_alianzas_duracion_discursos$periods_promedio <- round(seconds_to_period(by_alianzas_duracion_discursos$tiempo_promedio),2)




##########
########
# Indice de desproporcionalidad del uso del tiempo 
######################
#######################


agrupamientos_cupo2 <- by_cc_n_interventions_times2 %>% count(cupo2) %>% rename(n_constituyentes=n)



by_alianzas_duracion_discursos <-  by_alianzas_duracion_discursos %>% 
  left_join(agrupamientos_cupo2, by="cupo2")

names(by_alianzas_duracion_discursos)


by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>%
  select(cupo2,n_constituyentes, time_discursos, tiempo_promedio, n ) %>% rename(n_discursos=n)


by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>% rename(time_discursos_real=time_discursos)


number <- sum(by_alianzas_duracion_discursos$time_discursos_real)


by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>%
  mutate(tiempo_total=number)


by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>%
  mutate(prop_constituyentes=n_constituyentes/155)

by_alianzas_duracion_discursos <- by_alianzas_duracion_discursos %>%
  mutate(ideal_time_distri=tiempo_total*prop_constituyentes)

by_alianzas_duracion_discursos <-by_alianzas_duracion_discursos %>%
  mutate(prop_time_real=time_discursos_real/number)

by_alianzas_duracion_discursos <-by_alianzas_duracion_discursos %>%
  mutate(prop_time_ideal=ideal_time_distri/number)


by_alianzas_duracion_discursos <-by_alianzas_duracion_discursos %>%
  mutate(differ=abs(prop_time_real-prop_time_ideal),
         sumatoria_diferencias=sum(differ)/2) 


by_alianzas_duracion_discursos <-by_alianzas_duracion_discursos %>%
  rename(sumatoria_diferencias_div_2=sumatoria_diferencias)


by_alianzas_duracion_discursos <-by_alianzas_duracion_discursos %>%
  mutate(perct_desproporcionalidad_handby=sumatoria_diferencias_div_2*100)



#################

by_alianzas_duracion_discursos_df_sesion03 <- by_alianzas_duracion_discursos




by_alianzas_duracion_discursos_df_sesion04 <- by_alianzas_duracion_discursos
by_alianzas_duracion_discursos_df_sesion05 <- by_alianzas_duracion_discursos



by_alianzas_duracion_discursos_df_sesion06 <- by_alianzas_duracion_discursos


by_alianzas_duracion_discursos_df_sesion08 <- by_alianzas_duracion_discursos


by_alianzas_duracion_discursos_df_sesion09 <- by_alianzas_duracion_discursos

by_alianzas_duracion_discursos_df_sesion10 <- by_alianzas_duracion_discursos


by_alianzas_duracion_discursos_df_sesion11 <- by_alianzas_duracion_discursos

by_alianzas_duracion_discursos_df_sesion12 <- by_alianzas_duracion_discursos

by_alianzas_duracion_discursos_df_sesion13 <- by_alianzas_duracion_discursos



###########################


by_alianzas_duracion_discursos_df_sesion03$sesion <- 3
by_alianzas_duracion_discursos_df_sesion04$sesion <- 4
by_alianzas_duracion_discursos_df_sesion05$sesion <- 5
by_alianzas_duracion_discursos_df_sesion06$sesion <- 6
by_alianzas_duracion_discursos_df_sesion08$sesion <- 8
by_alianzas_duracion_discursos_df_sesion09$sesion <- 9
by_alianzas_duracion_discursos_df_sesion10$sesion <-10
by_alianzas_duracion_discursos_df_sesion11$sesion <- 11
by_alianzas_duracion_discursos_df_sesion12$sesion <- 12
by_alianzas_duracion_discursos_df_sesion13$sesion <- 13


byalianzas_desproporc_acumulada <- rbind(by_alianzas_duracion_discursos_df_sesion03,by_alianzas_duracion_discursos_df_sesion04,
                               by_alianzas_duracion_discursos_df_sesion05,by_alianzas_duracion_discursos_df_sesion06,
                               by_alianzas_duracion_discursos_df_sesion08,by_alianzas_duracion_discursos_df_sesion09,
                               by_alianzas_duracion_discursos_df_sesion10,by_alianzas_duracion_discursos_df_sesion11,
                               by_alianzas_duracion_discursos_df_sesion12, by_alianzas_duracion_discursos_df_sesion13)



byalianzas_desproporc <- rbind(by_alianzas_duracion_discursos_df_sesion03,by_alianzas_duracion_discursos_df_sesion04,
                               by_alianzas_duracion_discursos_df_sesion05,by_alianzas_duracion_discursos_df_sesion06,
                               by_alianzas_duracion_discursos_df_sesion08,by_alianzas_duracion_discursos_df_sesion09,
                               by_alianzas_duracion_discursos_df_sesion10,by_alianzas_duracion_discursos_df_sesion11,
                               by_alianzas_duracion_discursos_df_sesion12, by_alianzas_duracion_discursos_df_sesion13)



byalianzas_desproporc <- byalianzas_desproporc %>% select(sesion,perct_desproporcionalidad_handby) %>% 
  distinct()

byalianzas_desproporc_acumulada <- byalianzas_desproporc_acumulada %>% select(sesion,perct_desproporcionalidad_handby) %>% 
  distinct()


frames <- c(1,1,2,2,3,3,4,4,5,5)

byalianzas_desproporc_acumulada <- cbind(dd, frames)
byalianzas_desproporc_acumulada %>%
  ggplot(aes(sesion, perct_desproporcionalidad_handby)) + geom_histogram()

writexl::write_xlsx(byalianzas_desproporc_acumulada,"dd.xlsx")
p <- ggplot(
  byalianzas_desproporc_acumulada, 
  aes(x = sesion, y=..perct_desproporcionalidad_handby..)
) +
  stat_density(show.legend = FALSE, alpha = 0.7) +
  #scale_color_viridis_d() +
  #scale_size(range = c(3, 14)) +
  #scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

library(gganimate)
library(transformr)
p + transition_time(frames) 





## Static Plot
# ... date of event vs. interest in second date for women, men or couples
plot2 <- ggplot(data = byalianzas_desproporc_acumulada,
                aes(x = sesion, # date of speed dating event
                    y = perct_desproporcionalidad_handby # interest in 2nd date
                    #color = info, # which group: women/men/reciprocal
                    )) +
  geom_point(aes(group = seq_along(sesion), color=adjustcolor("purple",4), alpha=2), # needed, otherwise transition dosen't work
             size = 5, )+ # slightly transparent
  geom_line(aes(), color=adjustcolor("coral3",14.2)) + # slightly transparent
  labs(y = "% de Desproporcionalidad",
       x ="Sesiones",
       title = "Desproporcionalidad del tiempo usado en discursos") +

  scale_y_continuous(limits = c(0,70)) + scale_x_continuous(limits = c(3,13), breaks = c(3,4,5,6,7,8,9,10,11,12,13,14))+
  theme_bw() + 
  theme(legend.position = 'none',plot.margin = unit(c(1, 1, 3, 1), "lines"), axis.text.x=element_text(hjust=0,
                                                                            face = "bold", family = "bold", size=12),
        legend.text = element_text(size=12),
       
       axis.text.y=element_text(hjust=0, vjust=1, face = "bold",family = "bold", size = 12),
       axis.title.x =element_text(hjust=0.5,face = "bold", family = "bold", size=12),
       axis.title.y = element_text(hjust=0.5,face = "bold", family = "bold", size=12))  
plot2


class(plot2)
class(img_graph)
img_graph <- ggplot(img_graph)

logo <- system.file('input/telar_logo.png', package = 'patchwork')
logo <- png::readPNG("input/telar_logo.png", native = TRUE)

# adding image to graph 
img_graph <- plot2 +                  
  inset_element(p = logo, 0.889, 0.088, 0.99,0.171)
img_graph
ggsave(plot = img_graph,"output/plot_discursos_desproporcionalidad.png", width = 14, height = 11)
ggsave(plot = img_graph,"output/plot_discursos_desproporcionalidad.pdf")

## Animated Plot
p <- plot2 +
  transition_reveal(along = sesion) 





p


# animate in a two step process:
animate(p, height = 800, width =800)
anim_save("Gapminder_example2.gif")

# Save at gif:
anim_save("288-animated-barplot-transition.gif")

ggsave(plot = p,"plot.gift")



library(htmlwidgets)
library(animation)
# pass the animate command to saveHTML
saveHTML(animate(p, 
                 nframes = 10,         # fewer frames for simplification
                 device = "current"), 
         img.name = "gganimate_plot", 
         htmlfile = "gg.html")


data(gapminder, package = "gapminder")
gg <- ggplot(byalianzas_desproporc_acumulada, aes(sesion, perct_desproporcionalidad_handby, color = perct_desproporcionalidad_handby)) +
  geom_point(aes( frame = frames)) #+
  #scale_x_log10()
ggplotly(gg)




library(ggplotly)
library(ggridges)
theme_set(theme_minimal())
# Opened polygons
ggplot(byalianzas_desproporc_acumulada, aes(x = sesion, y = perct_desproporcionalidad_handby)) + 
  geom_density_ridges(fill = "#00AFBB")

# Closed polygons
ggplot(iris, aes(x = Sepal.Length, y = Species, group = Species)) + 
  geom_density_ridges2(fill = "#00AFBB")

# Cut off the trailin tails. 
# Specify `rel_min_height`: a percent cutoff
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(fill = "#00AFBB", rel_min_height = 0.01)




by_alianzas_duracion_discursos_df<- by_alianzas_duracion_discursos %>% select(cupo2, time_discursos_real, ideal_time_distri)

by_alianzas_duracion_discursos_df <- by_alianzas_duracion_discursos_df  %>%
  pivot_longer(!cupo2,names_to = "tipo", values_to="timed")

by_alianzas_duracion_discursos_df$timed_period <- round(seconds_to_period(by_alianzas_duracion_discursos_df$timed),2)
unique(by_alianzas_duracion_discursos_df$tipo)
by_alianzas_duracion_discursos_df$tipo <- factor(by_alianzas_duracion_discursos_df$tipo, levels = c("ideal_time_distri","time_discursos_real"))



plot_count_alianza <- by_alianzas_duracion_discursos_df %>% arrange(desc(timed))%>% 
  ggplot(aes(reorder(cupo2,desc(-timed)),timed, fill=as.factor(tipo))) + geom_bar(stat = "identity",position=position_dodge()) +
  scale_fill_manual(values = c("coral1","purple"),labels = c("Tiempo Proporcional", "Tiempo Real")) + 
  labs(title = "Tiempo proporcional y real por grupos",
       x="Alianza-Grupo coordinado",
       y="Tiempo en segundos")   + 
  geom_text(aes(label=timed_period),fontface = "bold", position=position_dodge(width=0.9), hjust=-0.25)+
  coord_flip() + theme_bw() + guides(fill=guide_legend(title="Tipo"))

plot_count_alianza



plot_count_alianza <- plot_count_alianza + theme(plot.margin = unit(c(1, 1, 3, 1), "lines"), axis.text.x=element_text(hjust=0,
                                                                                                                      face = "bold", family = "bold", size=12), legend.text = element_text(size=12),
                                                 
                                                 axis.text.y=element_text(hjust=0, vjust=1, face = "bold",family = "bold", size = 12),
                                                 axis.title.x =element_text(hjust=0.5,face = "bold", family = "bold", size=12),
                                                 axis.title.y = element_text(hjust=0.5,face = "bold", family = "bold", size=12)) 
plot_count_alianza

plot_count_alianza <- plot_count_alianza + theme(plot.title=element_text(size=14, face="bold", colour = "black"))+
  scale_y_continuous(limits = c(0,18000)) 

logo <- system.file('input/telar_logo.png', package = 'patchwork')
logo <- png::readPNG("input/telar_logo.png", native = TRUE)

# adding image to graph 
img_graph <- plot_count_alianza +                  
  inset_element(p = logo,
                0.88, 0.088, 0.99,0.197)


img_graph 


ggsave(plot = img_graph,"output/plot_discursos_times.png", width = 14, height = 11)

