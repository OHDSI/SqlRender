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

test_that("translate sql server -> DuckDB string concat", {
  sql <- translate("'x' + b ( 'x' + b)", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "'x' || b ( 'x' || b)")
})

test_that("translate sql server -> DuckDB string concat", {
  sql <- translate("a + ';b'", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "a || ';b'")
})

test_that("translate sql server -> DuckDB string concat", {
  sql <- translate("a + ';('", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "a || ';('")
})

test_that("translate sql server -> DuckDB add months", {
  sql <- translate("DATEADD(mm,2,date)", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "(date + TO_MONTHS(CAST(2 AS INTEGER)))"
  )
})

test_that("translate sql server -> DuckDB WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "duckdb"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> DuckDB WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "CREATE TABLE d AS\nSELECT\nc ;")
})

test_that("translate sql server -> DuckDB WITH INSERT INTO SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
    targetDialect = "duckdb"
  )
  expect_equal_ignore_spaces(
    sql,
    "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;"
  )
})

test_that("translate sql server -> DuckDB create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    targetDialect = "duckdb"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> DuckDB select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
    targetDialect = "duckdb"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> DuckDB temp table", {
  sql <- translate("SELECT * FROM #my_temp;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_temp;")
})

test_that("translate sql server -> DuckDB TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> DuckDB TOP subquery", {
  sql <- translate("SELECT name FROM (SELECT TOP 1 name FROM my_table WHERE a = b);",
    targetDialect = "duckdb"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT name FROM (SELECT name FROM my_table WHERE a = b LIMIT 1);"
  )
})

test_that("translate sql server -> DuckDB date to string", {
  sql <- translate("SELECT CONVERT(VARCHAR,start_date,112) FROM table;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "SELECT STRFTIME(start_date, '%Y%m%d') FROM table;")
})

test_that("translate sql server -> DuckDB CONVERT(AS DATE)", {
  sql <- translate("CONVERT(DATE, '20000101');", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "CAST(strptime('20000101', '%Y%m%d') AS DATE);"
  )
})

test_that("translate sql server -> DuckDB CONVERT(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "CAST(strptime('20000101', '%Y%m%d') AS DATE);"
  )
})

test_that("translate sql server -> DuckDB log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "SELECT (LN(CAST((number) AS REAL))/LN(CAST((base) AS REAL))) FROM table")
})

test_that("translate sql server -> DuckDB ISNUMERIC", {
  sql <- translate("SELECT CASE WHEN ISNUMERIC(a) = 1 THEN a ELSE b FROM c;",
    targetDialect = "duckdb"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CASE WHEN CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 1 THEN a ELSE b FROM c;"
  )
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT a FROM table WHERE CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 1"
  )
})

test_that("translate sql server -> DuckDB analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "ANALYZE results_schema.heracles_results;")
})

test_that("translate sql server -> DuckDB DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP, b TIMESTAMP);")
})

test_that("translate sql server -> DuckDB GETDATE", {
  sql <- translate("GETDATE()", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "CURRENT_DATE")
})

test_that("translate sql server -> DuckDB CREATE INDEX", {
  sql <- translate("CREATE INDEX idx_1 ON main.person (person_id);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "CREATE INDEX idx_1 ON main.person (person_id);")
})

test_that("translate sql server -> DuckDB DATEDIFF with literals", {
  sql <- translate("SELECT DATEDIFF(DAY, '20000131', '20000101');", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (CAST(strptime('20000101' , '%Y%m%d') AS DATE) - CAST(strptime('20000131' , '%Y%m%d') AS DATE));"
  )
})

test_that("translate sql server -> DuckDB DATEDIFF with date fields", {
  sql <- translate("SELECT DATEDIFF(DAY, date1, date2);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (CAST(date2 AS DATE) - CAST(date1 AS DATE));"
  )
})

test_that("translate sql server -> DuckDB DATEDIFF year with literals", {
  sql <- translate("SELECT DATEDIFF(YEAR, '20010131', '20000101');", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (EXTRACT(YEAR FROM CAST(strptime('20000101', '%Y%m%d') AS DATE)) - EXTRACT(YEAR FROM CAST(strptime('20010131', '%Y%m%d') AS DATE)));"
  )
})

test_that("translate sql server -> DuckDB DATEDIFF year with date fields", {
  sql <- translate("SELECT DATEDIFF(YEAR, date1, date2);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (EXTRACT(YEAR FROM CAST(date2 AS DATE)) - EXTRACT(YEAR FROM CAST(date1 AS DATE)));"
  )
})

test_that("translate sql server -> DuckDB DATEDIFF month literals", {
  sql <- translate("SELECT DATEDIFF(MONTH, '20000115', '20010116');", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (extract(year from age(CAST(strptime('20010116', '%Y%m%d') AS DATE), CAST(strptime('20000115', '%Y%m%d') AS DATE)))*12 + extract(month from age(CAST(strptime('20010116', '%Y%m%d') AS DATE), CAST(strptime('20000115', '%Y%m%d') AS DATE))));"
  )
})

test_that("translate sql server -> DuckDB DATEDIFF month date fields", {
  sql <- translate("SELECT DATEDIFF(MONTH, date1, date2);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (extract(year from age(CAST(date2 AS DATE), CAST(date1 AS DATE)))*12 + extract(month from age(CAST(date2 AS DATE), CAST(date1 AS DATE))));"
  )
})

test_that("translate sql server -> DuckDB CEILING", {
  sql <- translate("SELECT CEILING(0.1);", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "SELECT CEILING(0.1);")
})

test_that("translate sql server -> DuckDB DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test;")
})

test_that("translate sql server -> duckdb IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "duckdb")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table ;")
})

test_that("translate sql server -> DuckDB add days with period", {
  sql <- translate("DATEADD(DAY, -2.0, date)", targetDialect = "duckdb")
  expect_equal_ignore_spaces(
    sql,
    "(date + TO_DAYS(CAST(-2.0 AS INTEGER)))"
  )
})
