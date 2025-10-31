# data-raw/preparacion_datos.R

# Cargar todas las funciones del paquete
devtools::load_all()

# Vector con los IDs de estaciones
ids_estaciones <- c("NH0437", "NH0098", "NH0046", "NH0910", "NH0472")

# Función para preparar y guardar los datos
preparar_datos_estaciones <- function(ids_estaciones, carpeta_csv = "datosmeteo") {
  lista_datos <- list()

  # Crear carpeta si no existe (fuera de data-raw)
  if (!dir.exists(carpeta_csv)) {
    dir.create(carpeta_csv, recursive = TRUE)
    cli::cli_inform(c("v" = paste("Carpeta creada:", carpeta_csv)))
  }

  for (id_estacion in ids_estaciones) {
    cli::cli_h1(paste("Procesando estación", id_estacion))

    # Ruta donde se guardará el CSV
    ruta_csv <- file.path(carpeta_csv, paste0(id_estacion, ".csv"))

    # Descargar datos con tu función (ya definida en R/descarga_datos.R)
    datos_estacion <- descarga_data(id_estacion, ruta_csv)

    # Guardar en lista
    lista_datos[[id_estacion]] <- datos_estacion

    # Crear nombre de objeto dinámico
    nombre_objeto <- paste0("datos_", id_estacion)
    assign(nombre_objeto, datos_estacion, envir = .GlobalEnv)

    # Guardar .rda dentro de /data (nombre dinámico)
    do.call(usethis::use_data, list(as.name(nombre_objeto), overwrite = TRUE))
  }

  cli::cli_alert_success("✅ Todos los datasets fueron guardados en 'data/'.")
  cli::cli_alert_success("✅ Todos los archivos CSV fueron guardados en 'datosmeteo/'.")

  invisible(lista_datos)
}

# Ejecutar la preparación
preparar_datos_estaciones(ids_estaciones)
