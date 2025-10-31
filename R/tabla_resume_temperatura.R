#' Tabla resumidora de temperatura a 150 cm
#'
#' Genera una tabla de resumen estadístico de la variable 'temperatura_abrigo_150cm'
#' para una o más estaciones meteorológicas.
#'
#' @param datos Un data frame que contenga al menos las columnas 'id' y 'temperatura_abrigo_150cm'
#'
#'
#' @returns Un data frame con las columnas:
#' \itemize{
#' \item id - identificador de la estación.
#' \item promedio - temperatura promedio a 150 cm.
#' \item mediana - temperatura mediana a 150 cm.
#' \item minimo - temperatura mínima a 150 cm.
#' \item maximo - temperatura máxima a 150 cm. }
#' @examples
#' \dontrun{
#' datos<- descarga_data("NH0472", "datosmeteo/NH0472.csv")
#' tabla_resume_temperatura(datos)
#' }
#'
#'
#' @export
tabla_resume_temperatura <- function(datos) {
  datos |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      promedio = mean(temperatura_abrigo_150cm, na.rm = TRUE),
      mediana = median(temperatura_abrigo_150cm, na.rm = TRUE),
      minimo = min(temperatura_abrigo_150cm, na.rm = TRUE),
      maximo = max(temperatura_abrigo_150cm, na.rm = TRUE),
      .groups = "drop"
    )
}
