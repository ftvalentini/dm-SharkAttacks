source("src/load_librerias.R")

# operador para concatenar texto:
"%+%" <- function(a,b) paste(a,b,sep="")

# saca puntuacion y mayusculas de string:
clean <- function(x) trimws(tolower(gsub('[[:punct:] ]+',' ',x)))
