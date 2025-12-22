test_that("ajustar_lrp runs without error", {
  dose <- c(0, 50, 100, 150, 200)
  resp <- c(10, 15, 18, 19, 19.5)

  result <- ajustar_lrp2(dose, Y = resp, verbose = FALSE)
  expect_true("resultados" %in% names(result))
  expect_true(is.data.frame(result$resultados))
})
