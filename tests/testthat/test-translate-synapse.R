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

expect_match_ignore_spaces <- function(string1, regexp) {
  string1 <- gsub(" +", " ", string1)
  expect_match(string1, regexp)
}

test_that("translate sql server -> synapse CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 DATETIME CONSTRAINT a_c1_def DEFAULT GETDATE());",
    targetDialect = "synapse"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a(c1 DATETIME);"
  )
})

test_that("translate sql server -> synapse CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 DATETIME DEFAULT GETDATE());", targetDialect = "synapse")
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a(c1 DATETIME);"
  )
})

test_that("translate sql server -> synapse CREATE INDEX with WHERE", {
  sql <- translate("CREATE INDEX idx_a ON a(c1, c2) WHERE c3 <> '';", targetDialect = "synapse")
  expect_equal_ignore_spaces(sql, "CREATE INDEX idx_a ON a(c1, c2);")
})

test_that("translate sql server -> synapse IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "synapse")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table ;")
})
