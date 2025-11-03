#' Gráfico de temperatura mensual por estación
#'
#' Esta función genera un gráfico de líneas de la temperatura promedio mensual
#' a 150 cm de abrigo. Si se desea graficar más de una estación, se pueden
#' combinar los datasets usando `dplyr::bind_rows()` antes de llamar a esta función.
#'
#'
#' @param datos Un data frame con las columnas 'id', 'fecha', y 'temperatura_abrigo_150cm'.
#' @param colores Vector opcional de colores personalizados para las estaciones.
#' @param titulo Título del gráfico. Por defecto, "Temperatura".
#'
#' @returns Un objeto ggplot2 con el gráfico de temperaturas mensuales.
#'
#' @examples
#' \dontrun{
#' datos <- descarga_data("NH0472", "datosmeteo/NH0472.csv")
#' visualtm(datos)
#' }
#' @export
visualtm <- function(datos, colores = NULL, titulo = "Temperatura") {

  # Asegurarse de que fecha sea Date
  if(!inherits(datos$fecha, "Fecha")) {
    datos$fecha <- as.Date(datos$fecha)
  }

  # Crear columna mes
  datos <- dplyr::mutate(datos, mes = lubridate::month(fecha, label = TRUE, abbr = TRUE))

  # Calcular promedio mensual por estación
  resumen_mensual <- datos |>
    dplyr::group_by(id, mes) |>
    dplyr::summarise(promedio = mean(temperatura_abrigo_150cm, na.rm = TRUE), .groups = "drop")

  # Colores: si no se pasan suficientes, elegir aleatoriamente
  estaciones <- unique(resumen_mensual$id)
  if(is.null(colores) || length(colores) < length(estaciones)) {
    colores <- sample(grDevices::colors(), length(estaciones))
  }

  # Crear gráfico
  g <- ggplot2::ggplot(resumen_mensual, ggplot2::aes(x = mes, y = promedio, group = id, color = id)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = colores) +
    ggplot2::labs(title = titulo,
                  x = "Mes",
                  y = "Temperatura de abrigo 150cm",
                  color = "Estaci\u00F3n") +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(size = 12))

  return(g)
}


