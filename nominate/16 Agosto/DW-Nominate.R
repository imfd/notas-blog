#### 0. Carga de librerías requeridas ####
library(coda)
library(purrr)
library(tidyverse)
#library(tidytext)
library(wnominate)
library(wnomadds)
library(plotly)
library(htmlwidgets)
library(remotes)
#remotes::install_github('wmay/dwnominate')
library(dwnominate)

## Setting
range_base<-70
range_swap<-5
n_trials<-5
save_name<-"dw_max10"
n_minvotes<- 20  #default = 20

#### 1. Carga de bases de datos y join ####

# Carga objeto con votaciones (Fuente: sala.cconstituyente.cl)
votos_cc <- readRDS(url("https://storage.googleapis.com/notas-blog-public/nominate/votos_cc.rds"))

# Carga objeto con datos de convencionales constituyentes
legData_party_bol <- readRDS(url("https://storage.googleapis.com/notas-blog-public/nominate/legData_party_bol.rds"))

# Crea especificaciones de votos para generación de rollcalls y análisis Nominate. Se ordena votaciones cronológicamente
n_votes<-ncol(votos_cc)
votos_cc <- votos_cc[,c(n_votes:1)]
iteraciones<-(n_votes-range_base)%/%range_swap

#### 2. DW-Nominate ####

legData_party_bol$party<- legData_party_bol$grupo_actual
rollcalls<-list()
for(i in seq(0,iteraciones)){
  a<-i*range_swap
  rollcall_n<-list(rollcall(data = votos_cc[,c((1+a):(range_base+a))], 
                       legis.names = legData_party_bol$nombres_cc,
                       desc = "Votaciones CC",
                       legis.data = legData_party_bol,
                       yea = 1, nay = 6, missing = 9, notInLegis = NA))
  rollcalls[i] <-rollcall_n
  
}
resto<-(n_votes-range_base)%%range_swap
last_rollcat<-list(rollcall(data = votos_cc[,c((1+a+resto):(range_base+a+resto))], 
                       legis.names = legData_party_bol$nombres_cc,
                       desc = "Votaciones CC",
                       legis.data = legData_party_bol,
                       yea = 1, nay = 6, missing = 9, notInLegis = NA))
rollcalls[iteraciones+1]<-last_rollcat

#dw<-dwnominate(rollcalls, id="nombres_cc", dims = 1, polarity = 80)
dw2<-dwnominate(rollcalls, id="nombres_cc", dims = 2, polarity = c(80,137))


df_nominate2<-data.frame(names=str_to_title(legData_party_bol$nombres_cc),
                               Cupo=legData_party_bol$cupo_recod,
                               Grupo=legData_party_bol$grupo_actual,
                               Genero=legData_party_bol$genero,
                               Distrito=legData_party_bol$distrito,
                               x=dw2$legislators$coord1D,
                               y=dw2$legislators$coord2D,
                               Frame=dw2$legislators$session)

# Se invierte la polaridad de una de las dimensiones para asimilar ésta a eje izquierda-derecha
df_nominate2$y<-df_nominate2$y*(-1)

#### 3. Visualización ####

# Crea paleta de colores
paleta<-c("#d07200",
  "#6eceff",
  "#ff9144",
  "#00ad9f",
  "#fdc640",
  "#e41791",
  "#8259b2",
  "#f94144",
  "#90be6d",
  "#0082bd")

# Se configuran opciones de texto para el gráfico
f <- list(color = "black")
x <- list(title = "Primera dimensión",titlefont = f)
y <- list(title = "Segunda dimensión",titlefont = f)

##### 3.1 Gráfico plotly con versión dinámica ####
long_plot <- plot_ly(
  alpha=1,
  type="scatter",
  mode="markers",
  data = df_nominate2,
  size=4.5,
  x = ~x,
  y = ~y,
  #symbol=~Genero, 
  frame= ~Frame,
  color= ~Grupo,
  stroke = ~Grupo,
  strokes=paleta,
  alpha_stroke=1,
  text = ~paste("Nombre:",df_nominate2$names,"\n Distrito:",df_nominate2$Distrito),
  colors=paleta)%>%
  animation_opts(1500, redraw = FALSE) %>%
  layout(xaxis = x, yaxis = y,legend=list(title=list(text='<b>Grupo</b>')))%>% layout(
    images = list(
      source = base64enc::dataURI(file = "Data/telar_logo.png"),
      x = 1, y = 0.03, 
      sizex = 0.2, sizey = 0.1,
      xref = "paper", yref = "paper", 
      xanchor = "right", yanchor = "bottom"
    ),
    margin = list(t = 50)
  )

long_plot

# Guardado del gráfico dinámico
saveWidget(long_plot, paste0("./Products/",save_name,"_dinamic.html"), selfcontained = T, libdir = "lib")

##### 3.2 Visualización con KDE ####
p<-ggplot(df_nominate2, aes(x,y))+geom_point(aes(frame= Frame, ids=names,color=Grupo),alpha=0.5,size=2.5)+scale_color_manual(values=paleta)+geom_density_2d(aes(frame= Frame), color="#000000", alpha=0.05)+theme_bw()+theme(legend.title = element_blank())
p2<-ggplotly(p) %>% animation_opts(redraw = FALSE) %>%
  animation_opts(2000, easing = "cubic-in-out") %>%
  layout(xaxis = x, yaxis = y,legend=list(title=list(text='<b>Grupo</b>'))) %>%
  layout(
    images = list(
      source = base64enc::dataURI(file = "Data/telar_logo.png"),
      x = 1, y = 0.03, 
      sizex = 0.2, sizey = 0.1,
      xref = "paper", yref = "paper", 
      xanchor = "right", yanchor = "bottom"
    ),
    margin = list(t = 50)
  )%>%
  animation_button(label="Reproducir")
p2

#Guarda gráfico dinámico con KDE
saveWidget(p2, paste0("./Products/",save_name,"_dinamic_density.html"), selfcontained = T, libdir = "lib")


