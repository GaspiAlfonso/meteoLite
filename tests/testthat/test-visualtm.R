test_that("visualtm genera un gráfico correctamente", {
  datos_prueba <- data.frame(
    id = rep(c("A", "B"), each = 10),
    fecha = seq.Date(Sys.Date() - 9, Sys.Date(), by = "day"),
    temperatura_abrigo_150cm = runif(20, 10, 25)
  )

  grafico <- visualtm(datos_prueba)

  expect_true("ggplot" %in% class(grafico))
  expect_true(length(grafico$layers) > 0)
})

test_that("visualtm devuelve error si falta la columna fecha", {
  datos_malos <- data.frame(
    id = rep("A", 10),
    temperatura_abrigo_150cm = runif(10, 10, 25)
  )

  expect_error(visualtm(datos_malos))
})
