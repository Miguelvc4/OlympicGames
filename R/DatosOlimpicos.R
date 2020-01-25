#' @title Obtener Datos Historicos de los Juegos Olimpicos
#' @description Esta es una funcion para obtener los datos historicos de los juegos olimpicos
#' @details Esta funcion permite obtener los datos de los juegos Olimpicos desde el año 1896
#' Este Conjunto de Datos se ha procesado previamente (Limpiado, saneado y transformado)
#' @export
#' @import RCurl
#' @import tidyverse

ObtenerDatosOlimpicos <- function() {

  link <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQksRX5OLD_GS3bGCFDl5HeeWIhq-3MgHnH5j7HRqSBJlz18HmkQ9m4_W9Y5NnTgBQnKS8lXD3He1EW/pub?gid=175352860&single=true&output=csv"

  url <- getURL(link)
  con <- textConnection(url)
  DatosOlimpicos <- read.csv(con)
  levels(DatosOlimpicos$Medal)<-c("Gold","Silver","Bronze")

  DatosResumen <- DatosOlimpicos[,c("Name","Gender","Age","Height","Weight",
                                    "SportsCompetition","Medal","Region","Season","Year")]

  DatosEdad<- DatosOlimpicos %>% filter(!is.na(Age))

  save(DatosOlimpicos, file="./data/DatosOlimpicos.RData",version = 2, compress='xz')
  save(DatosResumen, file="./data/DatosResumen.RData",version = 2, compress='xz')
  save(DatosEdad, file="./data/DatosEdad.RData",version = 2, compress='xz')

  DatosOlimpicos
}




