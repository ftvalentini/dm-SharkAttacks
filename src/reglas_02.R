source("src/funciones.R")
source("src/load_librerias.R")

library(arules)

# carga base preprocesada -------------------------------------------------

base_t <- readRDS(file="data/final/base_procesada.rds")
# transforma base en clase "transacciones"
trans <- as(base_t, "transactions")
saveRDS(trans, "data/working/transacciones.RDS")

# transacciones con obs de ultimos 20 años y sin considerar tiempo
base_tb <- readRDS(file="data/final/base_procesada_B.rds")
transb <- as(base_tb, "transactions")



# explora -----------------------------------------------------------------
# nombre de todos los items
# itemInfo(trans)
# cantidad total de items:
(arules::itemFrequency(trans)) %>% length
# items que superan minsup:
(arules::itemFrequency(trans)>0.01) %>% sum
  # 41 items superan minsup(0.01)
# plot con items que superan minsup:
arules::itemFrequencyPlot(trans, support=0.01, cex.names=0.8, topN=41)
# plot con items que superan minsup en baseB:
arules::itemFrequencyPlot(transb, support=0.01, cex.names=0.8, topN=27)



# genera reglas -----------------------------------------------------------
reglas <- arules::apriori(trans,
                          parameter=list(support=0.01,
                                         confidence=0.3,
                                         minlen=2, 
                                         target="rules"))
# para baseB:
reglasb <- arules::apriori(transb,
                          parameter=list(support=0.01,
                                         confidence=0.3,
                                         minlen=2, 
                                         target="rules"))
sum_reglas <- arules::summary(reglas)
# nro de reglas generadas
sum_reglas@length
# tamaño de las reglas
sum_reglas@lengths
# caracteristicas (support, confidence, lift, frecuencia absoluta)
sum_reglas@quality
  # guarda tabla
sjPlot::tab_df(sum_reglas@quality, file="output/rules/summary_reglas.doc",
               use.viewer=F)

# reglas no redundantes para ambas bases
  # una regla es redundante si tiene el mismo consecuente que otra, 
  # menor o igual o confianza y un antecedente mas especifico
  # o sea, existe una regla mas general con mayor o igual confianza
reglas_nr <- reglas[!is.redundant(reglas)]
reglasb_nr <- reglasb[!is.redundant(reglasb)]

# saca conflicted porqqe hay qilombo
unloadNamespace("conflicted")

# reglas con mas lift
reglas_nr %>% arules::head(n=10, by="lift") %>% as("data.frame")
# reglas con mas confianza
reglas_nr %>% arules::head(n=10, by="confidence") %>% as("data.frame")

# Base B
# reglas con mas lift
reglasb_nr %>% arules::head(n=10, by="lift") %>% as("data.frame")
# reglas con mas confianza
reglasb_nr %>% arules::head(n=10, by="confidence") %>% as("data.frame")

# fatal en el consecuente ordenado por lift
reglas_nr %>% arules::subset(rhs %ain% "fatal=yes") %>% 
  arules::head(n=10, by="lift") %>% as("data.frame")
  # para base B
reglasb_nr %>% arules::subset(rhs %ain% "fatal=yes") %>% 
  arules::head(n=10, by="lift") %>% as("data.frame")


# visualizaciones ---------------------------------------------------------
library(arulesViz)
plot(reglas_nr, measure=c("support", "confidence"), shading="lift",
     control=list(main=NULL,col=colorspace::sequential_hcl(10)))

plot(reglas_nr, measure=c("support","confidence"), shading="order",
     control=list(main=NULL, col=rainbow(7)))

plot(reglas_nr %>% arules::subset(rhs %ain% "fatal=yes"), 
     measure=c("support","confidence"), shading="lift",
     control=list(main=NULL, col=colorspace::sequential_hcl(10)))
# revisar este:
plot(reglas_nr %>% arules::subset(rhs %ain% "fatal=yes"),
     method="grouped" )
# revisar este:
plot(reglasb_nr %>% arules::subset(rhs %ain% "fatal=yes"), 
     method="paracoord",
     control=list(main=NULL))



  