library(tidyverse) # para manipulación de datos y gráficos
library(plm) # para regresión efectos fijos
library(lmtest) # para función coeftest()

aportes <- read.csv('data/aportes.csv')

# Transformar los datos
aportes_apellido <- aportes %>%
  group_by(NOMBRE_CANDIDATO, PACTO, DISTRITO, FRECUENCIA_PATERNO)%>%
  summarise(MONTO = sum(MONTO), NSE_PATERNO = first(NSE_PATERNO), NSE_MATERNO = first(NSE_MATERNO)) %>%
  drop_na()

# Mantenemos solo las listas grandes. Las otras se agrupan.
listas <- c('VAMOS POR CHILE', 'LISTA DEL APRUEBO', 'APRUEBO DIGNIDAD')
aportes_apellido$lista <- as.character(aportes_apellido$PACTO)
aportes_apellido[!(aportes_apellido$PACTO %in% listas),]$lista <- "OTRO"
aportes_apellido$PACTO <- factor(aportes_apellido$lista)
aportes_apellido$PACTO <- relevel(aportes_apellido$PACTO, ref = 'OTRO')
aportes_apellido$lista <- NULL


fit_1 <- plm(MONTO ~ NSE_PATERNO + PACTO, data = aportes_apellido, index = 'DISTRITO', model = 'within')
summary(fit_1)

# Recalculamos significancia estadística con errores estándar robustos
coeftest(fit_1, vcov. = vcovHC, type = "HC1")
coef(fit_1)
within_intercept(fit_1)

write.csv(c(coef(fit_1)[1], within_intercept(fit_1)), 'coefs.csv')
###########################################################################################################################
#Compara listas

# En Vamos por Chile, incrementos en elitismo de apellido paga más que en otras listas
vamos <- aportes_apellido %>% filter(PACTO == 'VAMOS POR CHILE')
lm(MONTO ~ NSE_PATERNO, data = vamos)%>%
  summary()

apruebo <- aportes_apellido %>% filter(PACTO == 'LISTA DEL APRUEBO')
lm(MONTO ~ NSE_PATERNO, data = apruebo)%>%
  summary()

dignidad <- aportes_apellido %>% filter(PACTO == 'APRUEBO DIGNIDAD')
lm(MONTO ~ NSE_PATERNO, data = dignidad)%>%
  summary()

otros <- aportes_apellido %>% filter(PACTO == 'OTRO')
lm(MONTO ~ NSE_PATERNO, data = otros)%>%
  summary()


