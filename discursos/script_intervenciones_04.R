




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






#res_cc <- res %>% group_by(orador) %>% summarise(time=sum(seconds_duration))


#res_cc <- res_cc %>%  
 # left_join(otras_variables, by=c("orador"="nombres_cc"))
#sum(res_cc$time)



res_cc_distribution_time_distrito_gender <- res_cc %>%
  select(distrito, genero, orador, seconds_duration) %>%
  group_by(distrito, genero) %>%
  summarise(real_time=sum(seconds_duration))

res_cc_distrito_count <- otras_variables %>%
  count(distrito, genero) %>% 
  rowwise() %>%
  group_by(distrito) %>%
  mutate(prop_cons=(n/sum(n)))
  

names(res_cc_distribution_time_distrito_gender)
names(res_cc_distrito_count)


res_cc_distribution_time_distrito_gender <- res_cc_distribution_time_distrito_gender %>%
  left_join(res_cc_distrito_count %>% select(distrito,genero,prop_cons), by=c("distrito","genero"))

sum(res_cc_distribution_time_distrito_gender$real_time)

res_cc_distribution_time_distrito_gender <- res_cc_distribution_time_distrito_gender %>%
  group_by(distrito) %>%
  mutate(tiempo_total=sum(real_time))

res_cc_distribution_time_distrito_gender <- res_cc_distribution_time_distrito_gender %>%
  mutate(ideal_time=prop_cons*tiempo_total)


res_cc_distribution_time_distrito_gender <- res_cc_distribution_time_distrito_gender %>%
  mutate(prop_time_real=real_time/tiempo_total)


res_cc_distribution_time_distrito_gender <- res_cc_distribution_time_distrito_gender %>%
  mutate(prop_time_ideal=ideal_time/tiempo_total)


res_cc_distribution_time_distrito_gender <- res_cc_distribution_time_distrito_gender %>%
  mutate(differ=abs(prop_time_ideal-prop_time_real),
       sumatoria_diferencias=sum(differ)/2) 



res_cc_distribution_time_distrito_gender  <- res_cc_distribution_time_distrito_gender %>%
  mutate(perct_desproporcionalidad_handby=sumatoria_diferencias*100)


res_cc_distribution_time_distrito_gender_longer <- res_cc_distribution_time_distrito_gender %>%
  select(distrito, genero, real_time, ideal_time) %>%
  pivot_longer(!c(distrito, genero),names_to ="tipe_times", values_to="secs_times" )

######################################


res_cc_distribution_time_alianza_gender <- res_cc %>%
  select(cupo2.x, genero, orador, seconds_duration) %>%
  group_by(cupo2.x, genero) %>%
  summarise(real_time=sum(seconds_duration))

res_cc_alianza_count <- otras_variables %>%
  count(cupo2, genero) %>% 
  rowwise() %>%
  group_by(cupo2) %>%
  mutate(prop_cons=(n/sum(n)))



names(res_cc_distribution_time_alianza_gender)
names(res_cc_distrito_count)


res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  rename(cupo2=cupo2.x)

res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  left_join(res_cc_alianza_count %>% select(cupo2,genero,prop_cons), by=c("cupo2","genero"))

sum(res_cc_distribution_time_alianza_gender$real_time)

res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  group_by(cupo2) %>%
  mutate(tiempo_total=sum(real_time))

res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  mutate(ideal_time=prop_cons*tiempo_total)


res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  mutate(prop_time_real=real_time/tiempo_total)


res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  mutate(prop_time_ideal=ideal_time/tiempo_total)


res_cc_distribution_time_alianza_gender <- res_cc_distribution_time_alianza_gender %>%
  mutate(differ=abs(prop_time_ideal-prop_time_real),
         sumatoria_diferencias=sum(differ)/2) 



res_cc_distribution_time_alianza_gender  <- res_cc_distribution_time_alianza_gender %>%
  mutate(perct_desproporcionalidad_handby=sumatoria_diferencias*100)





################


res_cc_distribution_time_distrito_gender$label_time <- round(seconds_to_period(res_cc_distribution_time_distrito_gender$time),2)





plot_count_alianza <- res_cc_distribution_time_distrito_gender %>% arrange(desc(time))%>% 
  ggplot(aes(reorder(distrito,desc(-time)),time, fill=as.factor(genero))) + 
  geom_bar(stat = "identity",position="dodge2") +
  scale_fill_manual(values = c("coral1","purple"),labels = c("Femenino", "Masculino")) + 
  labs(title = "Distribución del tiempo por género y distrito",
       x="Alianza-Grupo coordinado",
       y="Tiempo en segundos")   + 
  geom_text(aes(label=label_time),size=3,fontface = "bold", position=position_dodge(width=0.9), hjust=-0.25)+
  coord_flip() + theme_bw() + guides(fill=guide_legend(title="Género"))

plot_count_alianza



plot_count_alianza <- plot_count_alianza + theme(plot.margin = unit(c(1, 1, 3, 1), "lines"), axis.text.x=element_text(hjust=0,
                                                                                                                      face = "bold", family = "bold", size=12), legend.text = element_text(size=12),
                                                 
                                                 axis.text.y=element_text(hjust=0, vjust=1, face = "bold",family = "bold", size = 12),
                                                 axis.title.x =element_text(hjust=0.5,face = "bold", family = "bold", size=12),
                                                 axis.title.y = element_text(hjust=0.5,face = "bold", family = "bold", size=12)) 
plot_count_alianza

plot_count_alianza <- plot_count_alianza + theme(plot.title=element_text(size=14, face="bold", colour = "black"))+
  scale_y_continuous(limits = c(0,5000)) 

logo <- system.file('input/telar_logo.png', package = 'patchwork')
logo <- png::readPNG("input/telar_logo.png", native = TRUE)

# adding image to graph 
img_graph <- plot_count_alianza +                  
  inset_element(p = logo,
                0.88, 0.088, 0.99,0.197)


img_graph 


ggsave(plot = img_graph,"output/plot_discursos_times_distrito_genero.png", width = 14, height = 11)





