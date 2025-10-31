test_that("tabla_resume_temperatura calcula medidas correctamente", {
  library(dplyr) # se carga solo en el entorno del test

  datos_prueba <- data.frame(
    id = c("A", "A", "B", "B"),
    temperatura_abrigo_150cm = c(20, 22, 18, 19)
  )

  resultado <- tabla_resume_temperatura(datos_prueba)

  # Esperamos 2 filas (una por id)
  expect_equal(nrow(resultado), 2)

  # Esperamos que las columnas sean las correctas
  expect_true(all(c("id", "promedio", "mediana", "minimo", "maximo") %in% names(resultado)))

  # Esperamos que el promedio de A sea 21
  expect_equal(resultado$promedio[resultado$id == "A"], 21)

  # Esperamos que no haya valores NA
  expect_false(any(is.na(resultado)))
})
