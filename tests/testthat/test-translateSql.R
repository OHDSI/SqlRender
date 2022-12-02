library("testthat")

test_that("translate: warning when using old function", {
  expect_warning(translateSql("SELECT * FROM my_table", "postgresql"))
})

test_that("translate: warning on table name that is too long", {
  expect_warning(translate("DROP TABLE abcdefghijklmnopqrstuvwxyz123456789", "pdw"))
})


test_that("translate sql server throws error when invalid target is given", {
  expect_error(translate("iSELECT * FROM a;", targetDialect = "pwd"))
})
