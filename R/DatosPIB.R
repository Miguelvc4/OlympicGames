
#' @title Obtener Datos Historicos de los Juegos Olimpicos
#' @description Esta es una funcion para obtener los datos historicos de los juegos olimpicos
#' @details Esta funcion permite obtener los datos de los juegos Olimpicos desde el año 1896
#' Este Conjunto de Datos se ha procesado previamente (Limpiado, saneado y transformado)
#' @export
#'
ObtenerDatosPIB <- function() {

  link <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSOaZqzcUMxsX2U0SuHvYLKVCWhXI1rWKHfr08XAh5wA5oDqM5wIzjIAHXLbg2W8QXK2qUjYOLuwnXE/pub?gid=946503257&single=true&output=csv"

  url <- getURL(link)
  con <- textConnection(url)
  DatosUnificados <- read.csv(con)
  DatosUnificados$PIB_LOG = round(log(DatosUnificados$PIB),2)

  save(DatosUnificados, file="./data/DatosUnificados.RData",version = 2, compress='xz')

}
