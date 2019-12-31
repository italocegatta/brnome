context("RANK")

test_that("brnome_rank works as expected", {

  expect_error(brnome_rank(sexo = "erro"), 'Indique o sexo apenas como "F" ou "M"')
  expect_error(brnome_rank(decada = 19022), 'Decada incorreta')
  expect_error(brnome_rank(decada = "erro"), 'Decada incorreta')
  expect_error(brnome_rank(decada = c(1980, 1990)), 'Indique apenas uma decada')

})
