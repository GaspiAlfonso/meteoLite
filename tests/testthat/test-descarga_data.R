test_that("descarga_data descarga y lee correctamente", {
  archivo <- tempfile(fileext = ".csv")
  datos <- descarga_data("NH0472", archivo)

  expect_true(is.data.frame(datos))  # en lugar de expect_s3_class
  expect_true(all(c("fecha", "temperatura_abrigo_150cm") %in% names(datos)))
  expect_gt(nrow(datos), 0)  # debería tener filas
})

test_that("descarga_data no descarga si el archivo ya existe", {
  archivo <- tempfile(fileext = ".csv")
  datos1 <- descarga_data("NH0472", archivo)
  datos2 <- descarga_data("NH0472", archivo)

  expect_equal(nrow(datos1), nrow(datos2))
})

