library("testthat")

# For debugging: force reload of patterns:
# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')

expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equivalent(string1, string2)
}

test_that("translate  -> sql server DROP TABLE IF EXISTS temp", {
  sql <- translate("DROP TABLE IF EXISTS #my_temp;", targetDialect = "sql server")
  expect_equal_ignore_spaces(sql, "IF OBJECT_ID('tempdb..#my_temp', 'U') IS NOT NULL DROP TABLE #my_temp;")
})

test_that("translate  -> sql server DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS cdm.dbo.table;", targetDialect = "sql server")
  expect_equal_ignore_spaces(sql, "IF OBJECT_ID('cdm.dbo.table', 'U') IS NOT NULL DROP TABLE cdm.dbo.table;")
})

test_that("translate  -> sql server CREATE TABLE IF NOT EXISTS", {
  sql <- translate("CREATE TABLE IF NOT EXISTS cdm.dbo.table (x INT);", targetDialect = "sql server")
  expect_equal_ignore_spaces(sql, "IF OBJECT_ID('cdm.dbo.table ', 'U') IS NULL CREATE TABLE cdm.dbo.table (x INT);")
})

test_that("translate sql server -> sql server temp dplyr ... pattern", {
  sql <- translate("SELECT * FROM cdm.dbo.my_table AS cdm.dbo.my_table...1;", targetDialect = "sql server")
  expect_equal_ignore_spaces(sql, "SELECT * FROM cdm.dbo.my_table AS cdmxdboxmy_tablexxx1;")
})
