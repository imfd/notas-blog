

library(tidyverse)
library(rtweet)
library(readr)
library(igraph)
library(ggplot2)



# load data
nodes_cc <- read_csv("https://storage.googleapis.com/notas-blog-public/04%20-%20Red%20Distrital%20Twitter/nodes_cc.csv") %>% select(-1)
edgelist_cc <- read_csv("https://storage.googleapis.com/notas-blog-public/04%20-%20Red%20Distrital%20Twitter/edgelist_cc.csv") %>% select(-1)
candidates_servel <- read_csv("https://storage.googleapis.com/notas-blog-public/04%20-%20Red%20Distrital%20Twitter/candidates_servel.csv")

# lookup all nodes

nodes_df <- lookup_users(nodes_cc$id)

# siempre he elegido colocar todo en minusculas para futuros joins

nodes_df$screen_name <- tolower(nodes_df$screen_name)


# juntamos la info de nuestros nodos con info actualizada de twitter, puede q algunos datos varien
nodes_info_tw <- nodes_cc %>%
  left_join(nodes_df, by=c("id"="screen_name"))

nodes_info_tw <- nodes_info_tw %>%
  filter(id!='robertoconstit1') # este candidato lo eliminamos porque bajó su cuenta hace pocos días

aux_filter_cservel <- nodes_info_tw%>%
  select(id_candidate,id)



candidates_servel_to_aux <- candidates_servel %>%
  select(id_candidate,territorio)

candidates_servel_to_aux <- candidates_servel_to_aux %>%
  left_join(aux_filter_cservel,by="id_candidate")
candidates_servel_to_aux <- candidates_servel_to_aux %>%
  mutate(id = replace_na(id, "Sin cuenta"),
         id = if_else(id!='Sin cuenta',"Con Cuenta", "Sin Cuenta"))

dis_accounts <- candidates_servel_to_aux %>%
  count(territorio, id) 
dis_accounts <- dis_accounts %>%
  group_by(territorio) %>%
  mutate(percent=(n/sum(n))*100 )
dis_accounts$percent <- round(dis_accounts$percent,2)

### calculamos promedio de seguidores y seguidos por distrito 

to_index <- nodes_info_tw %>%
  count(territorio)

index_by_dist <- nodes_info_tw %>%
  group_by(territorio) %>%
  summarise(sum_followers=(sum(followers_count)),
            sum_following=(sum(friends_count)))

index_by_dist <- index_by_dist %>%
  left_join(to_index, by="territorio")


# convertir followers a miles
index_by_dist$sum_followers_m <- index_by_dist$sum_followers/1e3

p <-index_by_dist %>%
  ggplot(aes(reorder(territorio,desc(-round(sum_followers_m,0))),sum_followers_m)) +
  geom_bar(aes(), stat='identity',fill='steelblue') + coord_flip() +
  geom_text(aes(label=round(sum_followers_m,0)),position=position_dodge(width=0.9), hjust=-0.25) +
  scale_y_continuous(limits = c(0,2000)) +
  xlab("Distrito") + ylab("Seguidores") + theme_bw()
p


# 
ggsave(plot=p, "output/plot_nota_followers.png" )




## creando red


## add districts 

edgelist_cc <- edgelist_cc %>%
  filter(user!='robertoconstit1') # lo removemos de nuestro edgelist_cc


#### añadimos info asociada a seguidores y seguidos

dist_user <- nodes_info_tw %>%
  select(id, territorio) %>%
  rename(territorio_user=territorio)

edgelist_cc_d <- edgelist_cc %>%
  left_join(dist_user, by=c("user"="id"))



dist_friend <- nodes_info_tw %>%
  select(id, territorio) %>%
  rename(territorio_friend=territorio)

edgelist_cc_d <- edgelist_cc_d %>%
  left_join(dist_friend, by=c("screen_name"="id"))


# seleccionamos variables del edge ahora representadas en los territorios distritaless

edgelist_cc_d <- edgelist_cc_d %>%
  select(territorio_user, territorio_friend)


### dataset nodes 

d1 <- data.frame(v1=unique(edgelist_cc_d$territorio_user))
d2 <- data.frame(v1=unique(edgelist_cc_d$territorio_friend))

nodes <- rbind(d1,d2)



nodes <- nodes %>%
  rename(id=v1)
nodes <- nodes %>%
  distinct()



# otorgamos una categoría a los nodos, asumiendo que el distrito 10 es una autoridad basada en la cantidad de followers que tienen

nodes <- nodes %>%
  mutate(dist=case_when(id=='DISTRITO 10'  ~ "Distritos Autoridad",
                        id!='DISTRITO 10' ~ "Otros Distritos"))




# cambiamos nombres para crear la red 
edgelist_cc_d <- edgelist_cc_d %>%
  select(territorio_user, territorio_friend) %>%
  rename(from=territorio_user, to=territorio_friend)


# red 
f1 <- graph_from_data_frame(edgelist_cc_d, nodes, directed = F)

f1 # 28 nodos y  12348 




V(f1)$color[V(f1)$dist=='Distritos Autoridad'] <- "red" # colorear nodo distinto
V(f1)$color[V(f1)$dist=='Otros Distritos'] <- "black" # colorear otros nodos



# asignamos el grado del nodo por la cantidad de entradas 
V(f1)$Popularity <- degree(f1, mode = 'in')


# le damos un layout
l <- layout_with_fr(f1)

#V(f1)$x <- l[,1] la rotación es opcional
#V(f1)$y <- l[,2]


# visualizamos la red 
jpeg(file = "output/Network_Distrital.jpg", width = 2100, height = 2100, units = "px", pointsize = 30, quality = 75, bg = "white", res = NA, family = "", restoreConsole = TRUE, type = c("windows", "cairo"))

plot(f1,
     layout=l,
     edge.color="gray90",
     edge.curved=T,
     vertex.color=V(f1)$color,
     vertex.label.color="black",
     vertex.label= V(f1)$name,
     vertex.size = V(f1)$Popularity*.003, vertex.label.size=.1)

legend("topleft",legend=c("Otros distritos", "Distrito más seguido"), pch=1,col=1:2, cex=0.8)

dev.off()
