




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



res$orador <- tolower(res$orador)
table(res$seconds_duration)


plot(res$seconds_duration)


res <- res %>% left_join(agrupamientos, by=c("orador"="nombres_cc"))




names(agrupamientos)
agrupamientos <- read_excel("input/agrupamientos.xlsx")


agrupamientos$cupo2[agrupamientos$cupo2=='Resto de lista']<- "Lis. Apruebo(resto)"
agrupamientos$cupo2[agrupamientos$cupo2=='Movimientos Soc. Const.']<- "MovimientosSoc.Const."
agrupamientos$cupo2[agrupamientos$cupo2=='Independientes']<- "Independientes(resto)"

otras_variables <- read_excel("input/df.xlsx")

agrupamientos$glosa_cand[agrupamientos$glosa_cand=='ELISA  LONCON ANTILEO'] <- "ELISA LONCON ANTILEO"

otras_variables <- otras_variables %>% left_join(agrupamientos , by=c("constituyente"="glosa_cand"))
names(otras_variables)
otras_variables <- otras_variables %>% select(5,2,1,9)




res_cc <- res %>%  
  left_join(otras_variables, by=c("orador"="nombres_cc"))



##########
########
# Indice de desproporcionalidad del uso del tiempo 
######################
#######################

# tabla de tiempos por distrito

distritos_times <- res_cc %>% group_by(distrito) %>% summarise(tiempo_real_distrito=sum(seconds_duration))
sum(distritos_times$tiempo_sumado_dis)
distritos_times$tiempo_total <- 70324

# tabla de constituyentes por distrito

distritos_n_constituyentes <- otras_variables %>% count(distrito)

distritos_n_constituyentes <- distritos_n_constituyentes  %>%
  group_by(distrito) %>% mutate(prop_constituyentes=n/155)



by_distrito_times <- distritos_n_constituyentes %>% rename(n_constituyentes=n) %>%
  left_join(distritos_times,by="distrito")



by_distrito_times <- by_distrito_times %>%
  mutate(tiempo_ideal_distrito=prop_constituyentes*tiempo_total)




by_distrito_times_df <- by_distrito_times %>% 
  select(distrito, tiempo_real_distrito, tiempo_ideal_distrito)



###

by_distrito_times_df <-by_distrito_times_df %>%
  mutate(prop_time_real=tiempo_real_distrito/70324)

by_distrito_times_df <-by_distrito_times_df %>%
  mutate(prop_time_ideal=tiempo_ideal_distrito/70324)


class(by_distrito_times_df)
by_distrito_times_df <- data.frame(by_distrito_times_df)

by_distrito_times_df <-by_distrito_times_df %>%
  mutate(differ=abs(prop_time_real-prop_time_ideal),
         sumatoria_diferencias=sum(differ)/2) 



by_distrito_times_df <-by_distrito_times_df %>%
  mutate(perct_desproporcionalidad_handby=sumatoria_diferencias*100)






##########


by_distrito_times_df$periods_time_real<- round(seconds_to_period(by_distrito_times_df$tiempo_real_distrito),2)

by_distrito_times_df$periods_time_ideal<- round(seconds_to_period(by_distrito_times_df$tiempo_ideal_distrito),2)


by_distrito_times_df2<- by_distrito_times_df %>% 
  select(distrito, tiempo_real_distrito, tiempo_ideal_distrito)

by_distrito_times_df2 <- by_distrito_times_df2  %>%
  pivot_longer(!distrito,names_to = "tipo", values_to="timed")

by_distrito_times_df2$timed_period <- round(seconds_to_period(by_distrito_times_df2$timed),2)


by_distrito_times_df3 <- by_distrito_times_df2 %>% filter(tipo=='tiempo_real_distrito')
by_distrito_times_df4 <- by_distrito_times_df2 %>% filter(tipo!='tiempo_real_distrito')

by_distrito_times_df4 <- by_distrito_times_df4%>%
  select(-2) %>% rename(tiempo_ideal=timed)

by_distrito_times_df3 <- by_distrito_times_df3 %>%
  left_join(by_distrito_times_df4, by="distrito")





plot_count_alianza <- by_distrito_times_df3 %>% arrange(desc(timed))%>% 
  ggplot(aes(reorder(distrito,desc(-timed)),timed, fill=as.factor(tipo))) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(y=tiempo_ideal, ymax=tiempo_ideal, ymin=tiempo_ideal),
                size=3, width=0.75 ) +
  scale_fill_manual(values = c("coral1","black"),labels = c("Tiempo Utilizado")) + 
  labs(title = "Tiempo proporcional y real por Distrito",
       subtitle = "Índice de desproporcionalidad=15.44%",
       x="Distrito",
       y="Tiempo en segundos")   + 
 # geom_text(aes(label=timed_period.x),fontface = "bold", position=position_dodge(width=0.9), hjust=-0.25)+
  coord_flip() + theme_bw() + guides(fill=guide_legend(title=""))

plot_count_alianza



plot_count_alianza <- plot_count_alianza + theme(plot.margin = unit(c(1, 1, 3, 1), "lines"), axis.text.x=element_text(hjust=0,
                                                                                                                      face = "bold", family = "bold", size=12), legend.text = element_text(size=12),
                                                 
                                                 axis.text.y=element_text(hjust=0, vjust=1, face = "bold",family = "bold", size = 12),
                                                 axis.title.x =element_text(hjust=0.5,face = "bold", family = "bold", size=12),
                                                 axis.title.y = element_text(hjust=0.5,face = "bold", family = "bold", size=12)) 
plot_count_alianza

plot_count_alianza <- plot_count_alianza + theme(plot.title=element_text(size=14, face="bold", colour = "black"))+
  scale_y_continuous(limits = c(0,10000)) 

logo <- system.file('input/telar_logo.png', package = 'patchwork')
logo <- png::readPNG("input/telar_logo.png", native = TRUE)

# adding image to graph 
img_graph <- plot_count_alianza +                  
  inset_element(p = logo,
                0.88, 0.088, 0.99,0.197)


img_graph 


ggsave(plot = img_graph,"output/plot_discursos_times_distritos.png", width = 14, height = 11)





