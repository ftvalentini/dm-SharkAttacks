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
                                         target="rules"),
                          appearance = list( none = c("sexo=unknown",
                                                      "fatal=unknown",
                                                      "horario=unknown",
                                                      "edad=unknown",
                                                      "Type=Invalid")))
# para baseB:
reglasb <- arules::apriori(transb,
                          parameter=list(support=0.01,
                                         confidence=0.3,
                                         minlen=2, 
                                         target="rules"),
                          appearance = list( none = c("sexo=unknown",
                                                      "fatal=unknown",
                                                      "horario=unknown",
                                                      "edad=unknown",
                                                      "Type=Invalid")))
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

# Caracterusticas luego de remover redundancia
sum_reglas_nr <- arules::summary(reglas_nr)
# nro de reglas generadas
sum_reglas_nr@length
# tamaño de las reglas
sum_reglas_nr@lengths
sjPlot::tab_df(as.data.frame(sum_reglas_nr@lengths), file="output/rules/length_reglas.doc",
               use.viewer=F)
sjPlot::
  # caracteristicas (support, confidence, lift, frecuencia absoluta)
sum_reglas_nr@quality
# guarda tabla
sjPlot::tab_df(sum_reglas_nr@quality, file="output/rules/summary_reglas.doc",
               use.viewer=F)
# saca conflicted porqqe hay qilombo
unloadNamespace("conflicted")


# reglas con mas soporte
reglas_nr %>% arules::head(n=20, by="support") %>% as("data.frame") %>%
write.csv(., file = "output/rules/reglas_nr_support.csv", sep = ",")


sjPlot::tab_df(reglas_nr %>% arules::head(n=10, by="support") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglas_nr_support.doc",
               use.viewer = F)

# reglas con mas lift
reglas_nr %>% arules::head(n=10, by="lift") %>% as("data.frame") %>% 
write.csv(., file = "output/rules/reglas_nr_lift.csv", sep = ",")

sjPlot::tab_df(reglas_nr %>% arules::head(n=10, by="lift") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglas_nr_lift.doc",
               use.viewer = F)

# reglas con mas confianza
reglas_nr %>% arules::head(n=10, by="confidence") %>% as("data.frame") 
sjPlot::tab_df(reglas_nr %>% arules::head(n=10, by="confidence") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglas_nr_confidence.doc",
               use.viewer = F)

# Base B
# Caracterusticas luego de remover redundancia
sum_reglasb_nr <- arules::summary(reglasb_nr)
# nro de reglas generadas
sum_reglasb_nr@length
# tamaño de las reglas
sum_reglasb_nr@lengths

sjPlot::tab_df(as.data.frame(sum_reglasb_nr@lengths), file="output/rules/length_reglasB.doc",
               use.viewer=F)
# caracteristicas (support, confidence, lift, frecuencia absoluta)
sum_reglasb_nr@quality
# guarda tabla
sjPlot::tab_df(sum_reglasb_nr@quality, file="output/rules/summary_reglasb.doc",
               use.viewer=F)

# reglas con mas soporte
reglasb_nr %>% arules::head(n=10, by="support") %>% as("data.frame") %>%
  write.csv(., file = "output/rules/reglasb_nr_support.csv", sep = ",")


sjPlot::tab_df(reglasb_nr %>% arules::head(n=10, by="support") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglasB_nr_support.doc",
               use.viewer = F)
# reglas con mas lift
reglasb_nr %>% arules::head(n=10, by="lift") %>% as("data.frame") %>% 
  write.csv(., file = "output/rules/reglasb_nr_lift.csv", sep = ",")

sjPlot::tab_df(reglasb_nr %>% arules::head(n=10, by="lift") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglasB_nr_lift.doc",
               use.viewer = F)

# reglas con mas confianza
reglasb_nr %>% arules::head(n=10, by="confidence") %>% as("data.frame")

sjPlot::tab_df(reglasb_nr %>% arules::head(n=10, by="confidence") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglasB_nr_confidence.doc",
               use.viewer = F)

# fatal en el consecuente ordenado por lift
reglas_nr %>% arules::subset(rhs %ain% "fatal=yes" & lhs %oin% "Type=Boating") %>% 
  arules::head(n=20, by="lift") %>% as("data.frame")

sjPlot::tab_df(reglas_nr %>% arules::subset(rhs %ain% "fatal=yes") %>% 
                 arules::head(n=20, by="lift") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglas_nr_fatalyes.doc",
               use.viewer = F)
  # para base B
reglasb_nr %>% arules::subset(rhs %ain% "fatal=yes") %>% 
  arules::head(n=20, by="lift") %>% as("data.frame")

sjPlot::tab_df(reglasb_nr %>% arules::subset(rhs %ain% "fatal=yes") %>% 
                 arules::head(n=20, by="lift") %>% as("data.frame") %>% mutate_if(is.numeric, round, digits = 3), file = "output/rules/reglasB_nr_fatalyes.doc",
               use.viewer = F)


# visualizaciones ---------------------------------------------------------
library(arulesViz)

# Base A

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
?plot

# Base B
  
plot(reglasb_nr, measure=c("support", "confidence"), shading="lift",
     control=list(main=NULL,col=colorspace::sequential_hcl(10)))

plot(reglasb_nr, measure=c("support","confidence"), shading="order",
     control=list(main=NULL, col=rainbow(7)))

plot(reglasb_nr %>% arules::subset(rhs %ain% "fatal=yes"), 
     measure=c("support","confidence"), shading="lift",
     control=list(main=NULL, col=colorspace::sequential_hcl(10)))

