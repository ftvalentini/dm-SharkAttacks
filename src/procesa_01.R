source("src/funciones.R")
source("src/load_librerias.R")


# carga base --------------------------------------------------------------

base_path <- "data/raw/Shark_Attack_Data.csv"
base_raw <- read.csv(base_path, header=T , sep=",", stringsAsFactors=F)

# preprocesamiento --------------------------------------------------------

# 29 countries mas frecuentes (dice 30 porque aparece "")
paises_top <- table(base_raw$Country) %>% base::sort(decreasing=T) %>% utils::head(30) %>%
  names %>% base::setdiff("")
# # 29 actividades mas frecuentes (dice 30 porque aparece "")
# activ_top <- table(clean(base_raw$Activity)) %>% 
#   base::sort(decreasing=T) %>% utils::head(30) %>% names %>% 
#   base::setdiff("")

# base preprocesada
base <-  base_raw %>%
  # drop variables irrelevantes
  select(-c(Case.Number.1, Case.Number.2, X,X.1, Name, Injury, Activity, Area,
            Location, Species, Investigator.or.Source, pdf, href.formula, href,
            Date, Case.Number, original.order)) %>% 
  # sexo = NA, F o M
  mutate(sexo = case_when(trimws(Sex) %in% "M" ~ "M", 
                          trimws(Sex) %in% "F" ~ "F",
                          TRUE ~ "unknown")) %>% 
  # region = 29 countries mas frecuentes u otros o unknown
  mutate(region = case_when(Country %in% paises_top ~ Country, 
                            Country %in% "" ~ "unknown",
                            TRUE ~ "other")) %>% 
  # # activity = 29 mas frecuentes + otros + unknown
  # mutate(region = case_when(Activity %in% paises_top ~ Country, 
  #                           Activity %in% c(""," ") ~ "unknown",
  #                           TRUE ~ "other")) %>% 
  # fatal = yes, no, unknown
  mutate(fatal = case_when(trimws(Fatal..Y.N.) %in% "Y" ~ "yes",
                           trimws(Fatal..Y.N.) %in% "N" ~ "no",
                           TRUE ~ "unknown")) %>% 
  # en Type "invalid" es "unknown"
  mutate(Type = case_when(Type %in% "Invalid" ~ "unknown",
                          TRUE ~ Type)) %>% 
  # horario = AM, PM, unknown (unknown incluye 82 casos de otros)
  mutate(
    horario = case_when(as.numeric(substr(Time,1,2)) %in% 0:11 ~ "AM",
                        as.numeric(substr(Time,1,2)) %in% 12:24 ~ "PM",
                        clean(Time) %in% c("afternoon","night",
                                    "late afternoon", "dusk",
                                    "evening","pm","early afternoon",
                                    "midnight","sunset","dark",
                                    "late afternoon") ~ "PM",
                        clean(Time) %in% c("morning", "am",
                                           "early morning","midday",
                                           "dawn", "just before noon") ~ "AM",
                        TRUE ~ "unknown"
    )) %>%
  # edad = young (<=15), adult, old(>50), unknown
  mutate(
    edad = case_when(as.numeric(substr(clean(Age),0,2)) <= 15 ~ "Young",
                     as.numeric(substr(clean(Age),0,2)) > 15 & 
                       as.numeric(substr(clean(Age),0,2)) <= 50 ~ "Adult",
                     as.numeric(substr(clean(Age),0,2)) > 50 ~ "Old",
                     clean(Age) %in% c("teen","teens",
                                       "\"young\"","both 11","young") ~ "Young",
                     clean(Age) %in% c("mid 30s","mid 20s",
                                       "ca 33","adult","middle age") ~ "Adult", 
                     clean(Age) %in% c("elderly") ~ "Old",
                     TRUE  ~ "unknown")
  ) %>% 
  # FILTRA casos con años imprecisos (0) o muy viejos (5, 77 y 500)
  # son 125 casos (2.1% de la base)
  dplyr::filter(Year > 500) %>% 
  # Year con bins equal length cada 10 años despues de 1800 (antes cada 100 años de 1500)
    # el ultimo periodo tiene 6 años
  mutate(
    year_bins = Hmisc::cut2(Year,c(seq(1500,1800,100), 
                                   seq(1810,2016,10)))
    ) %>% 
  # drop variables que sobran
  select(-c(Country, Sex, Fatal..Y.N., Time, Age))


# exploratorio ------------------------------------------------------------
# principales 4 regiones (para recateogrizar para explorar)
region_top <- table(base$region) %>% base::sort(decreasing=T) %>% utils::head(4) %>% names
# base para graficos
base_g <- base %>% 
  mutate(region = case_when(region %in% region_top ~ region,
                            region %in% "unknown" ~ region,
                            TRUE ~ "other")) %>% 
  mutate_if(is.character, as.factor)
# tabla descriptiva de las variables
summ_vec <- summary(base_g) %>% split(col(.)) %>% map(na.omit) %>% 
  map_chr(paste0,collapse=" / ")
tab_var <- data.frame(Variable=names(base_g),
                 Tipo="Nominal",
                 Descripcion="Hola",
                 Estadistica=summ_vec)
# guarda tabla
sjPlot::tab_df(tab_var, file="output/resumen_vars.docx",show.rownames=F,
               use.viewer=F)

# graficos ----------------------------------------------------------------

# barplots de las categoricas
bar_categ <- base_g %>% select(-c(Year,year_bins)) %>% tidyr::gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales="free",nrow=3) +
  geom_bar(stat="count") +
  labs(x=NULL, y=NULL,
       caption="En la distribución de Region los niveles poco frecuentes
       fueron agrupados en 'other'") +
  theme(plot.caption = element_text(hjust=0, size=9,colour="dimgray"))
ggsave(filename="output/barplots.png", bar_categ, width=9, height=7)

# distribucion de year segun sexo
hist_sexo <- ggplot(base_g, aes(x=Year, fill=sexo)) +
  geom_histogram(colour="black", binwidth=15) +
  facet_grid(sexo ~ ., scales="free", margins=T) +
  theme(legend.position="none") +
  labs(y=NULL, x=NULL)
ggsave(filename="output/hist_year_sexo.png", hist_sexo, width=7, height=4.2)

# distribucion de fatal segun year (post1850)
prop_fatal <- ggplot(base_g %>% dplyr::filter(Year>=1850), aes(x=Year, fill=fatal)) +
  geom_histogram(colour="black", binwidth=5, position="fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y=NULL)
ggsave(filename="output/prop_fatal.png", prop_fatal, width=6, height=3)

# proporcion de region segun años (post 1850)
prop_region <- base_g %>% dplyr::filter(Year>=1850) %>% 
  group_by(year_bins) %>%
  mutate(sum_y=n()) %>% 
  group_by(region, add=TRUE) %>%
  summarise(n=n()) %>% 
  mutate(per=n/sum(n)) %>% 
  ggplot(aes(x=year_bins, y=per, color=region, group=region)) +
  geom_line(cex=1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks=levels(base_g$year_bins)[seq(1,25,2)]) +
  labs(x=NULL, y=NULL)
ggsave(filename="output/prop_region.png", prop_region, width=8, height=3.2)

# proporcion de type segun años (post 1850)
prop_type <- base_g %>% dplyr::filter(Year>=1850) %>% 
  group_by(year_bins) %>%
  mutate(sum_y=n()) %>% 
  group_by(Type, add=TRUE) %>%
  summarise(n=n()) %>% 
  mutate(per=n/sum(n)) %>% 
  ggplot(aes(x=year_bins, y=per, color=Type, group=Type)) +
  geom_line(cex=1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks=levels(base_g$year_bins)[seq(1,25,2)]) +
  labs(x=NULL, y=NULL)
ggsave(filename="output/prop_type.png", prop_type, width=8, height=3.2)

# proporcion de edad segun años (post 1850)
prop_edad <- base_g %>% dplyr::filter(Year>=1850) %>% 
  group_by(year_bins) %>%
  mutate(sum_y=n()) %>% 
  group_by(edad, add=TRUE) %>%
  summarise(n=n()) %>% 
  mutate(per=n/sum(n)) %>% 
  ggplot(aes(x=year_bins, y=per, color=edad, group=edad)) +
  geom_line(cex=1) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks=levels(base_g$year_bins)[seq(1,25,2)]) +
  labs(x=NULL, y=NULL)
  # theme(axis.text.x=element_text(size=12))
ggsave(filename="output/prop_edad.png", prop_edad, width=8, height=3.2)


# relaciones entre categoricas --------------------------------------------

chi_df <- base_g %>% select_if(is.factor) %>% select(-c(year_bins)) %>% 
  # saca unknowns!
  mutate_if(is.factor, as.character) %>%   
  filter_all(all_vars(!(. == "unknown")))

# posibles combinaciones
chi_comb <- combn(1:ncol(chi_df), 2, simplify=F) %>% 
  map(function(x) names(chi_df[x]))
# tablas chisq (guarda un doc para cada tabla)
  # no pude guardar como png (la alternativa es html)
map(chi_comb,
    function(x) sjPlot::sjt.xtab(chi_df[[x[1]]], chi_df[[x[2]]],
                                 var.labels=x,
                                 show.row.prc=T,
                                 file="output/categorical/"%+%x[1]%+%"_"%+%x[2]%+%".doc",
                                 use.viewer=F)) 
# genera plots para ver relaciones entre categoricas
sjPlot::set_theme(axis.textsize.x=1)
chi_plots <- map(chi_comb,
          function(x) sjPlot::sjp.xtab(chi_df[[x[1]]], chi_df[[x[2]]],
                                       axis.titles=x[1],margin="row",
                                       show.prc=F,
                                       # show.total=F,
                                       legend.title=x[2]))
# guarda cada plot
walk2(chi_plots, chi_comb, function(x,y) 
  ggsave(filename="output/categorical/"%+%y[1]%+%"_"%+%y[2]%+%".png",
         plot=x$plot, width=9, height=4.3))


### otros graficos no usados:

# # distribucion de sex segun year (post1850)
# ggplot(base_g %>% dplyr::filter(Year>=1850), aes(x=Year, fill=sexo)) +
#   geom_histogram(colour="black", binwidth=5, position="fill") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   labs(y=NULL)

# # distribucion de year segun fatal
# ggplot(base_g, aes(x=Year, fill=fatal)) +
#   geom_histogram(colour="black", binwidth=15) +
#   facet_grid(fatal ~ ., scales="free") +
#   theme(legend.position="none") +
#   labs(y=NULL)

# # distribucion de year segun type
# ggplot(base_g, aes(x=Year, fill=Type)) +
#   geom_histogram(colour="black", binwidth=15) +
#   facet_grid(Type ~ ., scales="free") +
#   theme(legend.position="none") +
#   labs(y=NULL)

# ggplot(base_g, aes(x=Type, fill=sexo)) +
#   geom_bar(stat="count", position="fill")


# guarda base final -------------------------------------------------------

base_final <- base %>% select(-Year) %>% mutate_if(is.character, as.factor)
saveRDS(base_final, file="data/final/base_procesada.rds")

# version sin year_bins con los ultimos 20 años
base_final_b <- base %>% dplyr::filter(Year>1995) %>% 
  select(-c(Year,year_bins)) %>% mutate_if(is.character, as.factor)
saveRDS(base_final_b, file="data/final/base_procesada_B.rds")

# auxiliares --------------------------------------------------------------

# transformar age?
# age:
base$Age %>% table %>% base::sort(decreasing=T) %>% "["(which(.>1)) 


base$Year %>% hist
base$year_bins %>% table


# years anteriores a 1845
base_raw[base_raw$Year<1845,] %>% nrow
# hacer bins de year?



### auxiliar:
# cantidad maxima de unknowns incorrectos que se agregaron en horario:
base$horario[base$horario=="unknown"] %>% length -
  base_raw$Time[base_raw$Time=="" & base_raw$Year>=1845] %>% length
# cantidad maxima de unknown incorrectos asignados en edad:
(base$edad=="unknown") %>% sum - 
  (base_raw$Age %in% c(""," ") & base_raw$Year>500) %>% sum 
  


# auxiliar para horario
b <- base_raw$Time
b[as.numeric(substr(b, 1, 2)) %>% is.na] %>% table %>% base::sort(decreasing=T) %>% 
  "["(which(.>1)) %>% names
b[as.numeric(substr(b, 1, 2)) %>% is.na %>% "!"()] %>% table %>% base::sort(decreasing=T)
  # algunos horarios asignados incorrectamente




