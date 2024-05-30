library("testthat")

test_that("translate: warning when using old function", {
  expect_warning(translateSql("SELECT * FROM my_table", "postgresql"))
})

test_that("translate: warning on table name that is too long", {
  expect_warning(translate("DROP TABLE abcdefghijklmnopqrstuvwxyz1234567890123456789012345678901234567890", "pdw"))
})


test_that("translate sql server throws error when invalid target is given", {
  expect_error(translate("SELECT * FROM a;", targetDialect = "pwd"))
})

test_that("don't translate twice", {
  clearWarningBlock()
  sql <- "SELECT * INTO #temp FROM my_table;"
  sql <- translate(sql, targetDialect = "oracle")
  expect_warning(
    sql <- translate(sql, targetDialect = "postgresql"),
    "Input SQL has already been translated"
  )

  expect_equal(attr(sql, "sqlDialect"), "oracle")
})
