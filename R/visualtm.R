#' Gráfico de temperatura mensual por estación
#'
#' @param datos Un data frame con las columnas 'id', 'fecha', y 'temperatura_abrigo_150cm'.
#' @param colores Vector opcional de colores personalizados para las estaciones.
#' @param titulo Título del gráfico. Por defecto, "Temperatura".
#'
#' @returns Un objeto ggplot2 con el gráfico de temperaturas mensuales.
#'
#' @examples
#' \dontrun{
#' datos <- lectura_datos_estaciones("NH0472", "datos/NH0472.csv")
#' visualtm(datos)
#' }
#' @export
visualtm <- function(datos, colores = NULL, titulo = "Temperatura") {
  if(!require(dplyr)) stop("Cargá el paquete dplyr antes de usar esta función")
  if(!require(ggplot2)) stop("Cargá el paquete ggplot2 antes de usar esta función")
  if(!require(lubridate)) stop("Cargá el paquete lubridate antes de usar esta función")

  # Asegurarse de que fecha sea Date
  if(!inherits(datos$fecha, "Fecha")) {
    datos$fecha <- as.Date(datos$fecha)
  }

  # Crear columna mes
  datos <- datos %>%
    dplyr::mutate(mes = lubridate::month(fecha, label = TRUE, abbr = TRUE))

  # Calcular promedio mensual por estación
  resumen_mensual <- datos %>%
    dplyr::group_by(id, mes) %>%
    dplyr::summarise(promedio = mean(temperatura_abrigo_150cm, na.rm = TRUE),
                     .groups = "drop")

  # Colores: si no se pasan suficientes, elegir aleatoriamente
  estaciones <- unique(resumen_mensual$id)
  if(is.null(colores) || length(colores) < length(estaciones)) {
    colores <- sample(colors(), length(estaciones))
  }

  # Crear gráfico
  g <- ggplot(resumen_mensual, aes(x = mes, y = promedio, group = id, color = id)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = colores) +
    labs(title = titulo,
         x = "Mes",
         y = "Temperatura de abrigo 150cm",
         color = "Estación") +
    theme_minimal() +
    theme(text = element_text(size = 12))

  return(g)
}
