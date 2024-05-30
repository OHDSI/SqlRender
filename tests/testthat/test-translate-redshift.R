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

test_that("translate sql server -> RedShift VARCHAR(MAX)", {
  sql <- translate("VARCHAR(MAX)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "VARCHAR(MAX)")
})

test_that("translate sql server -> Redshift create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  IF NOT EXISTS  cohort\n  (cohort_definition_id INT)\nDISTSTYLE ALL;"
  )
})

test_that("translate sql server -> redshift datefromparts", {
  sql <- translate("SELECT DATEFROMPARTS(year,month,day) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT TO_DATE(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM'), 'YYYY-MM-DD') FROM table"
  )
})

test_that("translate sql server -> Redshift select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Redshift select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> RedShift SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROW_NUMBER() OVER STRTOL(LEFT(val, 15), 16) rn WHERE rn <= 1"
  )
})

test_that("translate sql server -> redshift hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE  #my_table\nDISTKEY(row_id)\nAS\nSELECT\n * \nFROM\n other_table;"
  )
})

test_that("translate sql server -> redshift hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE my_table (row_id INT);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE my_table (row_id INT)\nDISTKEY(row_id);"
  )
})

test_that("translate sql server -> redshift hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE  #my_table\nDISTSTYLE EVEN\nAS\nSELECT\n * \nFROM\n other_table;"
  )
})

test_that("translate sql server -> redshift hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE my_table (row_id INT);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE my_table (row_id INT)\nDISTSTYLE EVEN;"
  )
})


test_that("translate sql server -> redshift natural log", {
  sql <- translate("SELECT LOG(number) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((number) AS REAL)) FROM table")
})

test_that("translate sql server -> redshift log base 10", {
  sql <- translate("SELECT LOG10(number) FROM table;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT LOG(CAST((number) AS REAL)) FROM table;")
})

test_that("translate sql server -> redshift log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (LN(CAST((number) AS REAL))/LN(CAST(( base) AS REAL))) FROM table"
  )
})

test_that("translate sql server -> RedShift DATEADD dd", {
  sql <- translate("SELECT DATEADD(dd, 30, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(day, CAST(30 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD mm", {
  sql <- translate("SELECT DATEADD(mm, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(month, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD m", {
  sql <- translate("SELECT DATEADD(m, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(month, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD yyyy", {
  sql <- translate("SELECT DATEADD(yyyy, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(year, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD yy", {
  sql <- translate("SELECT DATEADD(yy, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(year, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD qq", {
  sql <- translate("SELECT DATEADD(qq, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(quarter, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD q", {
  sql <- translate("SELECT DATEADD(q, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(quarter, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD wk", {
  sql <- translate("SELECT DATEADD(wk, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(week, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD ww", {
  sql <- translate("SELECT DATEADD(ww, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(week, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD hh", {
  sql <- translate("SELECT DATEADD(hh, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(hour, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )

  sql <- translate("SELECT DATEADD(hour, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(hour, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD mi", {
  sql <- translate("SELECT DATEADD(mi, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(minute, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )

  sql <- translate("SELECT DATEADD(minute, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(minute, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD ss", {
  sql <- translate("SELECT DATEADD(ss, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(second, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )

  sql <- translate("SELECT DATEADD(second, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(second, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEADD mcs", {
  sql <- translate("SELECT DATEADD(mcs, 3, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEADD(microsecond, CAST(3 as int), drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF dd", {
  sql <- translate("SELECT DATEDIFF(dd, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF year", {
  sql <- translate("SELECT DATEDIFF(YEAR,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(YEAR,drug_era_start_date,drug_era_end_date) FROM drug_era;"
  )
})


test_that("translate sql server -> RedShift DATEDIFF m", {
  sql <- translate("SELECT DATEDIFF(m, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF mm", {
  sql <- translate("SELECT DATEDIFF(mm, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF yyyy", {
  sql <- translate("SELECT DATEDIFF(yyyy, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF yy", {
  sql <- translate("SELECT DATEDIFF(yy, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF qq", {
  sql <- translate("SELECT DATEDIFF(qq, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF q", {
  sql <- translate("SELECT DATEDIFF(q, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF wk", {
  sql <- translate("SELECT DATEDIFF(wk, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF ww", {
  sql <- translate("SELECT DATEDIFF(ww, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF hh", {
  sql <- translate("SELECT DATEDIFF(hh, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )

  sql <- translate("SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF mi", {
  sql <- translate("SELECT DATEDIFF(mi, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )

  sql <- translate("SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF n", {
  sql <- translate("SELECT DATEDIFF(n, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF ss", {
  sql <- translate("SELECT DATEDIFF(ss, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )

  sql <- translate("SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF mcs", {
  sql <- translate("SELECT DATEDIFF(mcs, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(microsecond, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG dd", {
  sql <- translate("SELECT DATEDIFF_BIG(dd, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG m", {
  sql <- translate("SELECT DATEDIFF_BIG(m, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG mm", {
  sql <- translate("SELECT DATEDIFF_BIG(mm, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG yyyy", {
  sql <- translate("SELECT DATEDIFF_BIG(yyyy, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG yy", {
  sql <- translate("SELECT DATEDIFF_BIG(yy, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG qq", {
  sql <- translate("SELECT DATEDIFF_BIG(qq, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG q", {
  sql <- translate("SELECT DATEDIFF_BIG(q, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG wk", {
  sql <- translate("SELECT DATEDIFF_BIG(wk, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG ww", {
  sql <- translate("SELECT DATEDIFF_BIG(ww, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG hh", {
  sql <- translate("SELECT DATEDIFF_BIG(hh, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG mi", {
  sql <- translate("SELECT DATEDIFF_BIG(mi, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG n", {
  sql <- translate("SELECT DATEDIFF_BIG(n, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG ss", {
  sql <- translate("SELECT DATEDIFF_BIG(ss, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEDIFF_BIG mcs", {
  sql <- translate("SELECT DATEDIFF_BIG(mcs, drug_era_start_date, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(microsecond, drug_era_start_date, drug_era_end_date) FROM drug_era;"
  )
})

test_that("translate sql server -> RedShift DATEPART dd", {
  sql <- translate("SELECT DATEPART(dd, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(day, drug_era_end_date) FROM drug_era;")

  sql <- translate("SELECT DATEPART(day, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(day, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART m", {
  sql <- translate("SELECT DATEPART(m, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(month, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART mm", {
  sql <- translate("SELECT DATEPART(mm, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(month, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART yyyy", {
  sql <- translate("SELECT DATEPART(yyyy, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(year, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART yy", {
  sql <- translate("SELECT DATEPART(yy, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(year, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART qq", {
  sql <- translate("SELECT DATEPART(qq, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(quarter, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART q", {
  sql <- translate("SELECT DATEPART(q, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(quarter, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART wk", {
  sql <- translate("SELECT DATEPART(wk, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(week, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART ww", {
  sql <- translate("SELECT DATEPART(ww, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(week, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART hh", {
  sql <- translate("SELECT DATEPART(hh, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(hour, drug_era_end_date) FROM drug_era;")

  sql <- translate("SELECT DATEPART(hour, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(hour, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART mi", {
  sql <- translate("SELECT DATEPART(mi, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")

  sql <- translate("SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART n", {
  sql <- translate("SELECT DATEPART(n, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART ss", {
  sql <- translate("SELECT DATEPART(ss, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(second, drug_era_end_date) FROM drug_era;")

  sql <- translate("SELECT DATEPART(second, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(second, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART mcs", {
  sql <- translate("SELECT DATEPART(mcs, drug_era_end_date) FROM drug_era;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(microsecond, drug_era_end_date) FROM drug_era;")
})


test_that("translate sql server -> RedShift DATETIMEFROMPARTS", {
  sql <- translate("SELECT DATETIMEFROMPARTS(year,month,day,hour,minute,second,millisecond) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(second,'00FM')||'.'||TO_CHAR(millisecond,'000FM') as TIMESTAMP) FROM table"
  )
})

test_that("translate sql server -> RedShift EOMONTH", {
  sql <- translate("SELECT EOMONTH(date) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT LAST_DAY(date) FROM table")
})

test_that("translate sql server -> RedShift VARIANCE", {
  sql <- translate("SELECT VAR(a) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT VARIANCE(a) FROM table")
})

test_that("translate sql server -> RedShift SQUARE", {
  sql <- translate("SELECT SQUARE(a + b) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT ((a + b) * (a + b)) FROM table")
})

test_that("translate sql server -> RedShift NEWID", {
  sql <- translate("SELECT NEWID()", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT MD5(RANDOM()::TEXT || GETDATE()::TEXT)")
})

test_that("translate sql server -> RedShift BOOL TYPE", {
  sql <- translate("CREATE TABLE table ( col BIT not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col BOOLEAN not null)")
})

test_that("translate sql server -> RedShift MONEY TYPE", {
  sql <- translate("CREATE TABLE table ( col MONEY not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col DECIMAL(19, 4) not null)")
})

test_that("translate sql server -> RedShift SMALLMONEY TYPE", {
  sql <- translate("CREATE TABLE table ( col SMALLMONEY not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col DECIMAL(10, 4) not null)")
})

test_that("translate sql server -> RedShift TINYINT TYPE", {
  sql <- translate("CREATE TABLE table ( col TINYINT not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col SMALLINT not null)")
})

test_that("translate sql server -> RedShift FLOAT TYPE", {
  sql <- translate("CREATE TABLE table ( col FLOAT(@s) not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col FLOAT not null)")
})

test_that("translate sql server -> RedShift DATETIME2 TYPE with precision specified", {
  sql <- translate("CREATE TABLE table ( col DATETIME2(@p) not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translate sql server -> RedShift DATETIME2 TYPE", {
  sql <- translate("CREATE TABLE table ( col DATETIME2 not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translate sql server -> RedShift DATETIME TYPE", {
  sql <- translate("CREATE TABLE table ( col DATETIME not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translate sql server -> RedShift SMALLDATETIME TYPE", {
  sql <- translate("CREATE TABLE table ( col SMALLDATETIME not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translate sql server -> RedShift DATETIMEOFFSET TYPE with precision specified", {
  sql <- translate("CREATE TABLE table ( col DATETIMEOFFSET(@p) not null)",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMPTZ not null)")
})

test_that("translate sql server -> RedShift DATETIMEOFFSET TYPE", {
  sql <- translate("CREATE TABLE table ( col DATETIMEOFFSET not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMPTZ not null)")
})

test_that("translate sql server -> RedShift TEXT TYPE", {
  sql <- translate("CREATE TABLE table ( col TEXT not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col VARCHAR(max) not null)")
})

test_that("translate sql server -> RedShift NTEXT TYPE", {
  sql <- translate("CREATE TABLE table ( col NTEXT not null)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col VARCHAR(max) not null)")
})

test_that("translate sql server -> RedShift UNIQUEIDENTIFIER TYPE", {
  sql <- translate("CREATE TABLE table ( col UNIQUEIDENTIFIER not null)",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col CHAR(36) not null)")
})

test_that("translate sql server -> RedShift STDEV POP", {
  sql <- translate("SELECT STDEVP(col) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT STDDEV_POP(col) FROM table")
})

test_that("translate sql server -> RedShift VAR POP", {
  sql <- translate("SELECT VARP(col) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT VAR_POP(col) FROM table")
})

test_that("translate sql server -> RedShift DATETIME2FROMPARTS", {
  sql <- translate("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds, 0, 0) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM') as TIMESTAMP) FROM table"
  )
})

test_that("translate sql server -> RedShift DATETIME2FROMPARTS with fractions", {
  sql <- translate("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds,fractions,precision) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0', precision) || 'FM') as TIMESTAMP) FROM table"
  )
})

test_that("translate sql server -> RedShift DATETIMEOFFSETFROMPARTS", {
  sql <- translate("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds, 0,h_offset,m_offset, 0) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table"
  )
})

test_that("translate sql server -> RedShift DATETIMEOFFSETFROMPARTS with fractions", {
  sql <- translate("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds,fractions,h_offset,m_offset,precision) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0',precision) || 'FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table"
  )
})

test_that("translate sql server -> RedShift GETUTCDATE", {
  sql <- translate("SELECT GETUTCDATE();", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translate sql server -> RedShift SMALLDATETIMEFROMPARTS", {
  sql <- translate("SELECT SMALLDATETIMEFROMPARTS(year,month,day,hour,minute) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM') as TIMESTAMP) FROM table"
  )
})

test_that("translate sql server -> RedShift SYSUTCDATETIME", {
  sql <- translate("SELECT SYSUTCDATETIME();", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translate sql server -> RedShift ATN2", {
  sql <- translate("SELECT ATN2(a, b) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT ATAN2(a, b) FROM table")
})

test_that("translate sql server -> RedShift TRUNCATION OF NUMBER", {
  sql <- translate("SELECT ROUND(expression,length,trunc) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT case when trunc = 0 then ROUND(CAST(expression AS FLOAT),length) else TRUNC(CAST(expression AS FLOAT),length) end FROM table"
  )
})

test_that("translate sql server -> RedShift CHARINDEX from position", {
  sql <- translate("SELECT CHARINDEX('test',column, 3) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT case when CHARINDEX('test', SUBSTRING(column, 3)) > 0 then (CHARINDEX('test', SUBSTRING(column, 3)) + 3 - 1) else 0 end FROM table"
  )
})

test_that("translate sql server -> RedShift QUOTENAME", {
  sql <- translate("SELECT QUOTENAME(a) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT QUOTE_IDENT(a) FROM table")
})

test_that("translate sql server -> RedShift SPACE", {
  sql <- translate("SELECT SPACE(n) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT REPEAT(' ',n) FROM table")
})

test_that("translate sql server -> RedShift STUFF", {
  sql <- translate("SELECT STUFF(expression, start, length, replace) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT SUBSTRING(expression, 0, start)|| replace||SUBSTRING(expression, start + length) FROM table"
  )
})

test_that("translate sql server -> RedShift CONCAT", {
  sql <- translate("SELECT CONCAT(p1,p2,p3,p4,p5,p6,p7) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT CONCAT(p1,CONCAT(p2,CONCAT(p3,CONCAT(p4,CONCAT(p5,CONCAT(p6,p7)))))) FROM table"
  )
})

test_that("translate sql server -> RedShift CONCAT", {
  sql <- translate("SELECT CONCAT('Condition occurrence record observed during long_term_days on or prior to cohort index:  ', CAST((p1.covariate_id-101)/1000 AS VARCHAR), '-', CASE WHEN c1.concept_name IS NOT NULL THEN c1.concept_name ELSE 'Unknown invalid concept' END) FROM table",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CONCAT('Condition occurrence record observed during long_term_days on or prior to cohort index:  ',CONCAT(CAST((p1.covariate_id-101)/1000 AS VARCHAR),CONCAT('-',CASE WHEN c1.concept_name IS NOT NULL THEN c1.concept_name ELSE 'Unknown invalid concept' END))) FROM table"
  )
})




test_that("translate sql server -> RedShift CTAS TEMP WITH CTE person_id", {
  sql <- translate("WITH a AS b SELECT person_id, col1, col2 INTO #table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id , col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS TEMP WITH CTE person_id at the end", {
  sql <- translate("WITH a AS b SELECT col1, col2, person_id INTO #table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  col1, col2, person_id\nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id", {
  sql <- translate("WITH a AS b SELECT person_id, col1, col2 INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id , col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias", {
  sql <- translate("WITH a AS b SELECT person_id as dist, col1, col2 INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id as dist, col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias at the end", {
  sql <- translate("WITH a AS b SELECT col1, col2, person_id as dist INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, col2, person_id as dist \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias (no 'as')", {
  sql <- translate("WITH a AS b SELECT col1, person_id dist, col2 INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, person_id dist, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias (no 'as') at the end", {
  sql <- translate("WITH a AS b SELECT col1, col2, person_id dist INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, col2, person_id dist \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS TEMP person_id", {
  sql <- translate("SELECT person_id, col1, col2 INTO #table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nSELECT\n  person_id , col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS person_id", {
  sql <- translate("SELECT person_id, col1, col2 INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(person_id)\nAS\nSELECT\n  person_id , col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS person_id with alias", {
  sql <- translate("SELECT person_id as dist, col1, col2 INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n  person_id as dist, col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS person_id with alias at the end", {
  sql <- translate("SELECT col1, col2, person_id as dist INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n col1, col2, person_id as dist \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS person_id with alias (no 'as')", {
  sql <- translate("SELECT person_id dist, col1, col2 INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n  person_id dist, col1, col2 \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CTAS person_id with alias (no 'as') at the end", {
  sql <- translate("SELECT col1, col2, person_id dist INTO table FROM person;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n col1, col2, person_id dist \nFROM\n person;"
  )
})

test_that("translate sql server -> RedShift CREATE TABLE person_id", {
  sql <- translate("CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  [dbo].[drug_era]  ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nDISTKEY(person_id);"
  )
})

test_that("translate sql server -> RedShift ISDATE", {
  sql <- translate("SELECT * FROM table WHERE ISDATE(col) = 1", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT * FROM table WHERE REGEXP_INSTR(col, '^(\\\\d{4}[/\\-]?[01]\\\\d[/\\-]?[0123]\\\\d)([ T]([0-1][0-9]|[2][0-3]):([0-5][0-9])(:[0-5][0-9](.\\\\d+)?)?)?$') = 1"
  )
})

test_that("translate sql server -> RedShift ISNUMERIC", {
  sql <- translate("SELECT * FROM table WHERE ISNUMERIC(col) = 1", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT * FROM table WHERE REGEXP_INSTR(col, '^[\\-\\+]?(\\\\d*\\\\.)?\\\\d+([Ee][\\-\\+]?\\\\d+)?$') = 1"
  )
})

test_that("translate sql server -> RedShift PATINDEX", {
  sql <- translate("SELECT PATINDEX(pattern,expression) FROM table;", targetDialect = "redshift")
  expect_equal_ignore_spaces(
    sql,
    "SELECT REGEXP_INSTR(expression, case when LEFT(pattern,1)<>'%' and RIGHT(pattern,1)='%' then '^' else '' end||TRIM('%' FROM REPLACE(pattern,'_','.'))||case when LEFT(pattern,1)='%' and RIGHT(pattern,1)<>'%' then '$' else '' end) FROM table;"
  )
})

test_that("translate sql server -> RedShift SELECT INTO temp table with CTE and default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste("WITH cte(a1) AS (SELECT a1 FROM table_a)",
    "SELECT *",
    "INTO #table",
    "FROM cte;",
    sep = " "
  ), targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste("CREATE TABLE  #table  DISTSTYLE ALL",
    "AS",
    "WITH",
    " cte(a1) ",
    "AS",
    " (SELECT a1 FROM table_a) ",
    "SELECT",
    " * ",
    "FROM",
    " cte;",
    sep = "\n"
  ))
})

test_that("translate sql server -> RedShift SELECT INTO permanent table with CTE and default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste("WITH cte(a1) AS (SELECT a1 FROM table_a)",
    "SELECT *",
    "INTO table",
    "FROM cte;",
    sep = " "
  ), targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste("CREATE TABLE  table  DISTSTYLE ALL",
    "AS",
    "WITH",
    " cte(a1) ",
    "AS",
    " (SELECT a1 FROM table_a) ",
    "SELECT",
    " * ",
    "FROM",
    " cte;",
    sep = "\n"
  ))
})

test_that("translate sql server -> RedShift SELECT INTO temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste("SELECT *", "INTO #table", "FROM another_table;", sep = " "),
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, paste("CREATE TABLE  #table  DISTSTYLE ALL",
    "AS",
    "SELECT",
    " * ",
    "FROM",
    " another_table;",
    sep = "\n"
  ))
})

test_that("translate sql server -> RedShift SELECT INTO permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste("SELECT *", "INTO table", "FROM another_table;", sep = " "),
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, paste("CREATE TABLE  table  DISTSTYLE ALL",
    "AS",
    "SELECT",
    " * ",
    "FROM",
    " another_table;",
    sep = "\n"
  ))
})

test_that("translate sql server -> RedShift SELECT value INTO temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translate("SELECT a INTO #table;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE  #table DISTSTYLE ALL\nAS\nSELECT\n a ;")
})

test_that("translate sql server -> RedShift SELECT value INTO permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translate("SELECT a INTO table;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE  table DISTSTYLE ALL\nAS\nSELECT\n a ;")
})

test_that("translate sql server -> RedShift create temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translate("CREATE TABLE #table (id int not null, col varchar(max));",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  #table  (id int not null, col varchar(max))\nDISTSTYLE ALL;"
  )
})

test_that("translate sql server -> RedShift create permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translate("CREATE TABLE table (id int not null, col varchar(max));",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE  table  (id int not null, col varchar(max))\nDISTSTYLE ALL;"
  )
})

test_that("translate sql server -> RedShift CREATE TABLE IF NOT EXISTS with hashing", {
  sql <- translate(paste("IF OBJECT_ID('dbo.heracles_results', 'U') IS NULL",
    "CREATE TABLE dbo.heracles_results",
    "(",
    "cohort_definition_id int,",
    "analysis_id int,",
    "stratum_1 varchar(255),",
    "stratum_2 varchar(255),",
    "stratum_3 varchar(255),",
    "stratum_4 varchar(255),",
    "stratum_5 varchar(255),",
    "count_value bigint,",
    "last_update_time datetime",
    ");",
    sep = "\n"
  ), targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste("CREATE TABLE  IF NOT EXISTS  dbo.heracles_results",
    "(cohort_definition_id int,",
    "analysis_id  int,",
    "stratum_1 varchar(255),",
    "stratum_2 varchar(255),",
    "stratum_3 varchar(255),",
    "stratum_4 varchar(255),",
    "stratum_5 varchar(255),",
    "count_value bigint,",
    "last_update_time TIMESTAMP",
    ")",
    "DISTKEY(analysis_id);",
    sep = "\n"
  ))
})

test_that("translate sql server -> RedShift DISTINCT + TOP", {
  sql <- translate("SELECT DISTINCT TOP 100 * FROM table WHERE a = b;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT TOP 100 DISTINCT * FROM table WHERE a = b;")
})

test_that("RedShift XOR operator", {
  sql <- translate("select a ^ b from c where a = 1;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select a # b from c where a = 1;")
})

test_that("translate sql server -> redshift hint DISTKEY + SORTKEY on CTAS + CTE", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(COMPOUND:start_date)\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT * INTO #my_table FROM cte;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(COMPOUND:start_date)\nCREATE TABLE #my_table\nDISTKEY(row_id)\nCOMPOUND SORTKEY(start_date)\nAS\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT\n * \nFROM\n cte;"
  )
})

test_that("translate sql server -> redshift hint SORTKEY on CTAS + CTE", {
  sql <- translate("--HINT SORT_ON_KEY(COMPOUND:start_date)\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT * INTO #my_table FROM cte;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT SORT_ON_KEY(COMPOUND:start_date)\nCREATE TABLE #my_table\nCOMPOUND SORTKEY(start_date)\nAS\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT\n * \nFROM\n cte;"
  )
})

test_that("translate sql server -> redshift hint DISTKEY + SORTKEY on CTAS", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(:start_date, end_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(:start_date, end_date)\nCREATE TABLE #my_table\nDISTKEY(row_id)\nSORTKEY(start_date, end_date)\nAS\nSELECT\n*\nFROM\n other_table;"
  )
})

test_that("translate sql server -> redshift hint SORTKEY on CTAS", {
  sql <- translate("--HINT SORT_ON_KEY(:start_date, end_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT SORT_ON_KEY(:start_date, end_date)\nCREATE TABLE #my_table\nSORTKEY(start_date, end_date)\nAS\nSELECT\n * \nFROM\n other_table;"
  )
})

test_that("translate sql server -> redshift hint DISTKEY + SORTKEY on CREATE TABLE", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date)\nDISTKEY(row_id)\nINTERLEAVED SORTKEY(start_date);"
  )
})

test_that("translate sql server -> redshift hint SORTKEY on CREATE TABLE", {
  sql <- translate("--HINT SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date)\nINTERLEAVED SORTKEY(start_date);"
  )
})

test_that("translate sql server -> redshift CONVERT to DATE", {
  sql <- translate("select CONVERT(DATE, start_date) from my_table;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select CAST(start_date as DATE) from my_table;")
})

test_that("translate sql server -> redshift CONVERT to TIMESTAMPTZ", {
  sql <- translate("select CONVERT(TIMESTAMPTZ, start_date) from my_table;",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select CONVERT(TIMESTAMP WITH TIME ZONE, start_date) from my_table;"
  )
})

test_that("translate sql server -> Redshift partition window function sorted descending", {
  sql <- translate("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt desc) as count_value",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  DESC ROWS UNBOUNDED PRECEDING) as count_value"
  )
})

test_that("translate sql server -> Redshift partition window function sorted ascending", {
  sql <- translate("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt asc) as count_value",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  ASC ROWS UNBOUNDED PRECEDING) as count_value"
  )
})

test_that("translate sql server -> Redshift partition window function no sort specified", {
  sql <- translate("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt) as count_value",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  ROWS UNBOUNDED PRECEDING) as count_value"
  )
})

test_that("translate sql server -> Redshift partition window function with specified frame", {
  sql <- translate("select MAX(start_ordinal) OVER (PARTITION BY groupid ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select MAX(start_ordinal) OVER (PARTITION BY groupid ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal"
  )
})

test_that("translate sql server -> Redshift partition window function ROW_NUMBER no sort specified", {
  sql <- translate("select ROW_NUMBER() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select ROW_NUMBER() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function CUME_DIST no sort specified", {
  sql <- translate("select CUME_DIST() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select CUME_DIST() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function DENSE_RANK no sort specified", {
  sql <- translate("select DENSE_RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select DENSE_RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function PERCENT_RANK no sort specified", {
  sql <- translate("select PERCENT_RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select PERCENT_RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function RANK no sort specified", {
  sql <- translate("select RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function LAG no sort specified", {
  sql <- translate("select LAG(mycol) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select LAG(mycol) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function LEAD no sort specified", {
  sql <- translate("select LEAD(mycol) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select LEAD(mycol) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift partition window function NTILE no sort specified", {
  sql <- translate("select NTILE(4) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select NTILE(4) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function sorted descending without partition by clause", {
  sql <- translate("select sum(count(person_id)) over (order by prc_cnt desc) as count_value",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select sum(count(person_id)) OVER (ORDER BY prc_cnt  DESC ROWS UNBOUNDED PRECEDING) as count_value"
  )
})

test_that("translate sql server -> Redshift window function sorted ascending without partition by clause", {
  sql <- translate("select sum(count(person_id)) over (order by prc_cnt asc) as count_value",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select sum(count(person_id)) OVER (ORDER BY prc_cnt  ASC ROWS UNBOUNDED PRECEDING) as count_value"
  )
})

test_that("translate sql server -> Redshift window function no sort specified without partition by clause", {
  sql <- translate("select sum(count(person_id)) over (order by prc_cnt) as count_value",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select sum(count(person_id)) OVER (ORDER BY prc_cnt ROWS UNBOUNDED PRECEDING) as count_value"
  )
})

test_that("translate sql server -> Redshift window function ROW_NUMBER no sort specified without PARTITION BY clause", {
  sql <- translate("select ROW_NUMBER() over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select ROW_NUMBER() OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function CUME_DIST no sort specified without PARTITION BY clause", {
  sql <- translate("select CUME_DIST() over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select CUME_DIST() OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function DENSE_RANK no sort specified without PARTITION BY clause", {
  sql <- translate("select DENSE_RANK() over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select DENSE_RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function PERCENT_RANK no sort specified without PARTITION BY clause", {
  sql <- translate("select PERCENT_RANK() over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select PERCENT_RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function RANK no sort specified without PARTITION BY clause", {
  sql <- translate("select RANK() over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function LAG no sort specified without PARTITION BY clause", {
  sql <- translate("select LAG(mycol) over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select LAG(mycol) OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function LEAD no sort specified without PARTITION BY clause", {
  sql <- translate("select LEAD(mycol) over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select LEAD(mycol) OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift window function NTILE no sort specified without PARTITION BY clause", {
  sql <- translate("select NTILE(4) over (procedure_concept_id ORDER BY prc_cnt) as num",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(
    sql,
    "select NTILE(4) OVER (procedure_concept_id ORDER BY prc_cnt) as num"
  )
})

test_that("translate sql server -> Redshift clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "-- redshift does not support indexes")
})

test_that("translate sql server -> Redshift index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "redshift"
  )
  expect_equal_ignore_spaces(sql, "-- redshift does not support indexes")
})

test_that("translate sql server -> Redshift analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "ANALYZE results_schema.heracles_results;")
})

test_that("translate sql server -> redshift DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x  (a TIMESTAMP, b TIMESTAMP)\nDISTSTYLE ALL;")
})

test_that("translate sql server -> redshift DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test;")
})

test_that("translate sql server -> redshift drvd()", {
  sql <- translate("SELECT
      TRY_CAST(name AS VARCHAR(MAX)) AS name,
      TRY_CAST(speed AS FLOAT) AS speed
    FROM (  VALUES ('A', 1.0), ('B', 2.0)) AS drvd(name, speed);", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT\n      CAST(name AS VARCHAR(MAX)) AS name,\n      CAST(speed AS FLOAT) AS speed\n    FROM (SELECT NULL AS name, NULL AS speed WHERE (0 = 1) UNION ALL SELECT 'A', 1.0 UNION ALL SELECT 'B', 2.0) AS values_table;")
})

# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')
