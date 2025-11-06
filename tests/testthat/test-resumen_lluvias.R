test_that("resumen_lluvias calcula medidas correctamente", {
  library(dplyr)   # se carga solo en el entorno del test
  library(lubridate)

  # Datos de prueba
  datos_prueba <- data.frame(
    id = c("A", "A", "A", "B", "B"),
    fecha = as.Date(c("2024-01-01", "2024-01-15", "2024-02-01", "2024-01-10", "2024-02-05")),
    precipitacion_pluviometrica = c(10, 0, 5, 20, NA)
  )

  resultado <- resumen_lluvias(datos_prueba)

  # Esperamos que tenga las columnas correctas
  expect_true(all(c("id", "mes", "lluvia_total", "lluvia_promedio", "lluvia_ocurrencia") %in% names(resultado)))

  # Esperamos 4 filas (dos meses por dos IDs)
  expect_equal(nrow(resultado), 4)

  # Verificamos cálculos para id = "A" en enero
  res_A_ene <- resultado |> filter(id == "A", mes == ymd("2024-01-01"))
  expect_equal(res_A_ene$lluvia_total, 10)
  expect_equal(res_A_ene$lluvia_promedio, 5)
  expect_equal(res_A_ene$lluvia_ocurrencia, 1)

  # Verificamos cálculos para id = "A" en febrero
  res_A_feb <- resultado |> filter(id == "A", mes == ymd("2024-02-01"))
  expect_equal(res_A_feb$lluvia_total, 5)
  expect_equal(res_A_feb$lluvia_promedio, 5)
  expect_equal(res_A_feb$lluvia_ocurrencia, 1)

  # Verificamos cálculos para id = "B" en enero
  res_B_ene <- resultado |> filter(id == "B", mes == ymd("2024-01-01"))
  expect_equal(res_B_ene$lluvia_total, 20)
  expect_equal(res_B_ene$lluvia_promedio, 20)
  expect_equal(res_B_ene$lluvia_ocurrencia, 1)

  # Verificamos cálculos para id = "B" en febrero
  res_B_feb <- resultado |> filter(id == "B", mes == ymd("2024-02-01"))
  expect_equal(res_B_feb$lluvia_total, 0)
  expect_true(is.na(res_B_feb$lluvia_promedio))
  expect_equal(res_B_feb$lluvia_ocurrencia, 0)

  # No deberían haber NAs en las columnas de ocurrencia ni total
  expect_false(any(is.na(resultado$lluvia_total)))
  expect_false(any(is.na(resultado$lluvia_ocurrencia)))
})


test_that("resumen_lluvias lanza error si faltan columnas requeridas", {
  datos_incompletos <- data.frame(
    id = c(1, 2),
    fecha = as.Date(c("2024-01-01", "2024-02-01"))
  )

  expect_error(resumen_lluvias(datos_incompletos))
})
