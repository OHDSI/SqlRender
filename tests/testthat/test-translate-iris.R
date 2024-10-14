library(testthat)
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

# tests wrt string concatenation
test_that("translate sql server -> InterSystems IRIS string concatenation", {
  sql <- translate("SELECT CONCAT(a, 'b', c)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT a || 'b' || c")
})
test_that("translate sql server -> InterSystems IRIS string concatenation", {
  sql <- translate("SELECT CONCAT(a, 'b', c, d, e, e, f)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT a || 'b' || c || d || e || e || f")
})
test_that("translate sql server -> InterSystems IRIS string +", {
  sql <- translate("SELECT CAST(a AS VARCHAR) + CAST(b AS VARCHAR(10)) + CAST(c AS VARCHAR) + 'd'", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT CAST(a AS VARCHAR) || CAST(b AS varchar(10)) || CAST(c AS VARCHAR) || 'd'")
})
test_that("translate sql server -> InterSystem IRIS string concatenation DOB", {
  sql <- translate("SELECT CONCAT(p.year_of_birth, 11, 11)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT p.year_of_birth||'-'||11||'-'||11")
})


# build date from parts
test_that("translate sql server -> InterSystems IRIS DATEFROMPARTS()", {
  sql <- translate("SELECT DATEFROMPARTS(yyyy, mm, dd)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(TO_CHAR(yyyy,'FM0000')||'-'||TO_CHAR(mm,'FM00')||'-'||TO_CHAR(dd,'FM00'), 'YYYY-MM-DD')")
})
test_that("translate sql server -> InterSystems IRIS DATETIMEFROMPARTS()", {
  sql <- translate("SELECT DATETIMEFROMPARTS(yyyy, mm, dd, hh, mi, ss, ms)", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_TIMESTAMP(TO_CHAR(yyyy,'FM0000')||'-'||TO_CHAR(mm,'FM00')||'-'||TO_CHAR(dd,'FM00')||' '||TO_CHAR(hh,'FM00')||':'||TO_CHAR(mi,'FM00')||':'||TO_CHAR(ss,'FM00')||'.'||TO_CHAR(ms,'FM000'), 'YYYY-MM-DD HH24:MI:SS.FF')")
})



# temp table handling
test_that("translate sql server -> InterSystems IRIS implicit CTAS", {
  sql <- translate("SELECT a, b INTO t_new FROM t;", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "CREATE TABLE t_new AS SELECT a, b FROM t;")
})
test_that("translate sql server -> InterSystems IRIS implicit CTTAS", {
  sql <- translate("SELECT a, b INTO #t_new FROM t;", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, paste("CREATE GLOBAL TEMPORARY TABLE ", getTempTablePrefix(), "t_new AS SELECT a, b FROM t;", sep=""))
})


# test DATEADD() flavours
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(d, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(d,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(dd, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(dd,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(day, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(day,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(m, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(m,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(mm, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(mm,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(yy, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(yy,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})
test_that("translate sql server -> InterSystems IRIS DATEADD(d, ..)", {
  sql <- translate("SELECT DATEADD(yyyy, 1, '2007-07-28') AS dt", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE(DATEADD(yyyy,1,'2007-07-28'),'YYYY-MM-DD HH:MI:SS') AS dt")
})


# test reserved words
test_that("translate sql server -> InterSystems IRIS reserved word DOMAIN", {
  sql <- translate("SELECT t.domain, 'domain' FROM omopcdm.domain AS t", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT t.\"DOMAIN\", 'domain' FROM omopcdm.\"DOMAIN\" AS t")
})
test_that("translate sql server -> InterSystems IRIS reserved words for aggregates", {
  sql <- translate("SELECT MIN(x) AS min, MAX(x) AS max, COUNT(x) as COUNT FROM t", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT MIN(x) AS \"MIN\", MAX(x) AS \"MAX\", COUNT(x) AS \"COUNT\" FROM t")
})


# test function names
test_that("translate sql server -> InterSystems IRIS function names", {
  sql <- translate("SELECT STDEV(x), STDEV_POP(x), STDEV_SAMP(x), EOMONTH(dt) FROM t", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT STDDEV(x), STDDEV_POP(x), STDDEV_SAMP(x), LAST_DAY(dt) FROM t")
})


# test FROM (VALUES ... )
test_that("translate sql server -> InterSystems IRIS FROM ( VALUES ... ) clause", {
  sql <- translate("SELECT * FROM (SELECT TRY_CAST(a AS INT) AS a, TRY_CAST(b AS DOUBLE) AS b FROM (VALUES (1, 2), (2, 3)) AS drvd(a, b);", targetDialect = "iris")
  expect_equal_ignore_spaces(sql, "SELECT * FROM (SELECT CAST(a AS INT) AS a, CAST(b AS DOUBLE) AS b FROM ((SELECT NULL AS a, NULL AS b WHERE (0 = 1)) UNION ALL (SELECT 1, 2) UNION ALL (SELECT 2, 3)) AS values_table;")
})
