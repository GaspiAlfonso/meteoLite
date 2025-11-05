#' Resumen de precipitaciones por mes
#'
#' Calcula estadisticas basicas de precipitacion a partir de un data set con las variables requeridas.
#' Devuelve un resumen con el total de lluvia diaria,
#' cantidad de días con precipitación y promedio por estación.
#'
#' @param datos Un data frame que contenga al menos las columnas 'id', 'fecha' y 'precipitacion_pluviometrica'.
#'
#'
#' @returns Un data frame con las columnas:
#' \itemize{
#' \item id - identificador de la estación.
#' \item fecha
#' \item lluvia_total - cantidad total en el mes.
#' \item lluvia_promedio - promedio diario a lo largo del mes.
#' \item lluvia_ocurrencia - cantidad de dias que llovio.}
#' @examples
#' \dontrun{
#' resumen_lluvias(datos)
#' }
#'
#'
#' @export
resumen_lluvias <- function(datos) {

  stopifnot(all(c("id", "fecha", "precipitacion_pluviometrica") %in% names(datos)))

  datos |>
    dplyr::mutate(mes = lubridate::floor_date(fecha, "month")) |>
    dplyr::group_by(id, mes) |>
    dplyr::summarise(
      lluvia_total = sum(precipitacion_pluviometrica, na.rm = TRUE),
      lluvia_promedio = mean(precipitacion_pluviometrica, na.rm = TRUE),
      lluvia_ocurrencia = sum(precipitacion_pluviometrica > 0, na.rm = TRUE),
      .groups = "drop"
    )
}
