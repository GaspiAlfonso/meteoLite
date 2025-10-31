#' Descargar y leer datos de estaciones meteorológicas
#'
#' Esta función descarga (si es necesario) y lee los datos de distintas estaciones meteorológicas
#' a partir de su ID.
#'
#'
#' @param id_estacion Código de la estación, por ejemplo "NH0472"
#' @param ruta Ruta donde se guardará el archivo descargado, por ejemplo "datos/NH0472.csv"
#'
#' @returns Un data frame con los datos de la estación
#' @examples
#' \dontrun{
#' datos <- descarga_data("NH0472", "datos/NH0472.csv")
#' }
#' @export
descarga_data <- function(id_estacion, ruta) {
  url <- paste0("https://raw.githubusercontent.com/rse-r/intro-programacion/main/datos/", id_estacion, ".csv")

  if (!file.exists(ruta)) {
    cli::cli_inform(c("i" = "El archivo no existe en la ruta especificada."))
    cli::cli_inform(c("i" = "Descargando datos desde GitHub..."))
    download.file(url, ruta, mode = "wb")
    cli::cli_inform(c("v" = "Archivo descargado correctamente."))
  } else {
    cli::cli_inform(c("v" = "El archivo ya existe. No se descarga de nuevo."))
  }

  cli::cli_inform(c("i" = "Leyendo datos..."))
  datos <- readr::read_csv(ruta, show_col_types = FALSE)
  cli::cli_inform(c("v" = "Datos cargados en R."))

  datos$estacion <- id_estacion

  return(datos)
}

