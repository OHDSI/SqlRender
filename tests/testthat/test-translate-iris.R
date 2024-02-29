library(testthat)
library(devtools)
library(rJava)


# For debugging: force reload of code & patterns:
# load_all()
# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('../../inst/csv/replacementPatterns.csv')


expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equivalent(string1, string2)
}

test_that("translate sql server -> InterSystems IRIS string concatenation", {
  sql <- translate("SELECT CONCAT(a, 'b', c)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT a || 'b' || c")
})
test_that("translate sql server -> InterSystems IRIS string +", {
  sql <- translate("SELECT CAST(a AS VARCHAR) + CAST(b AS VARCHAR(10)) + CAST(c AS VARCHAR) + 'd'", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT CAST(a AS VARCHAR) || CAST(b AS varchar(10)) || CAST(c AS VARCHAR) || 'd'")
})


test_that("translate sql server -> InterSystems IRIS DATEFROMPARTS()", {
  sql <- translate("SELECT DATEFROMPARTS(yyyy, mm, dd)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(TO_CHAR(yyyy,'FM0000')||'-'||TO_CHAR(mm,'FM00')||'-'||TO_CHAR(dd,'FM00'), 'YYYY-MM-DD')")
})
test_that("translate sql server -> InterSystems IRIS DATETIMEFROMPARTS()", {
  sql <- translate("SELECT DATETIMEFROMPARTS(yyyy, mm, dd, hh, mi, ss, ms)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_TIMESTAMP(TO_CHAR(yyyy,'FM0000')||'-'||TO_CHAR(mm,'FM00')||'-'||TO_CHAR(dd,'FM00')||' '||TO_CHAR(hh,'FM00')||':'||TO_CHAR(mi,'FM00')||':'||TO_CHAR(ss,'FM00')||'.'||TO_CHAR(ms,'FM000'), 'YYYY-MM-DD HH24:MI:SS.FF')")
})



test_that("translate sql server -> InterSystems IRIS implicit CTAS", {
  sql <- translate("SELECT a, b INTO t_new FROM t;", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "CREATE TABLE t_new AS SELECT a, b FROM t;")
})
test_that("translate sql server -> InterSystems IRIS implicit CTTAS", {
  sql <- translate("SELECT a, b INTO #t_new FROM t;", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, paste("CREATE GLOBAL TEMPORARY TABLE ", getTempTablePrefix(), "t_new AS SELECT a, b FROM t;", sep=""))
})
