# Cargamos los datos del paquete
devtools::load_all

# Descargamos los datos de la estación NH0472
datos_NH0472 <- descarga_data("NH0472", "datosmeteo/NH0472.csv")

# Guardamos el dataset dentro del paquete
usethis::use_data(datos_NH0472, overwrite = TRUE)

