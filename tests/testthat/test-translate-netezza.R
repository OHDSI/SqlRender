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

test_that("translate sql server -> Netezza select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Netezza select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY hash(CAST(person_id AS VARCHAR(1000)))) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Netezza SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER hex_to_binary(val) rn WHERE rn <= 1")
})

test_that("translate sql server -> Netezza WITH cte AS () INSERT INTO tbl SELECT * FROM cte", {
  sql <- translate("WITH data AS (SELECT 'test' AS user, 'secret' AS password) INSERT INTO users SELECT * FROM data;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT INTO users WITH data AS (SELECT 'test' AS user, 'secret' AS password) SELECT * FROM data;"
  )
})

test_that("translate sql server -> Netezza CAST(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101' , 'yyyymmdd');")
})

test_that("translate sql server -> Netezza DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;"
  )
})

test_that("translate sql server -> Netezza DATEDIFF year", {
  sql <- translate("SELECT DATEDIFF(YEAR,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT (DATE_PART('YEAR', CAST(drug_era_end_date AS DATE)) - DATE_PART('YEAR', CAST(drug_era_start_date AS DATE))) FROM drug_era;"
  )
})

test_that("translate sql server -> Netezza DATEDIFF(MONTH)", {
  sql <- translate("SELECT DATEDIFF(month,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "SELECT MONTHS_BETWEEN(CAST(drug_era_end_date AS DATE), CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translate sql server -> Netezza DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
})

test_that("translate sql server -> Netezza WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Netezza WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1  AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> Netezza WITH CTE SELECT INTO with RANDOM distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nWITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE d \nAS\nWITH cte1  AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1\nDISTRIBUTE ON RANDOM;"
  )
})

test_that("translate sql server -> Netezza WITH CTE SELECT INTO with KEY distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(c)\nWITH cte1 AS (SELECT a,c FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(c)\nCREATE TABLE d \nAS\nWITH cte1  AS (SELECT a,c FROM b)  SELECT\nc \nFROM\ncte1\nDISTRIBUTE ON (c);"
  )
})

test_that("translate sql server -> Netezza WITH SELECT INTO with RANDOM distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nSELECT a INTO b FROM someTable;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE b \nAS\nSELECT\na \nFROM\nsomeTable\nDISTRIBUTE ON RANDOM;"
  )
})

test_that("translate sql server -> Netezza WITH SELECT INTO with KEY distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(a)\nSELECT a INTO b FROM someTable;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(a)\nCREATE TABLE b \nAS\nSELECT\na \nFROM\nsomeTable\nDISTRIBUTE ON (a);"
  )
})

test_that("translate sql server -> Netezza SELECT INTO TEMP TABLE", {
  sql <- translate("SELECT a INTO #b;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "CREATE TEMP TABLE b\n AS \n SELECT \n a;")
})

test_that("translate sql server -> Netezza SELECT INTO TABLE", {
  sql <- translate("SELECT a INTO b;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "CREATE TABLE b \n AS \n SELECT a;")
})

test_that("translate sql server -> Netezza DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "DROP TABLE cohort IF EXISTS;")
})

test_that("translate sql server -> Netezza LEFT functions", {
  sql <- translate("SELECT LEFT(x,4);", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x, 1, 4);")
})

test_that("translate sql server -> Netezza RIGHT functions", {
  sql <- translate("SELECT RIGHT(x,4);", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x, LENGTH(x) - 4 + 1, 4);")
})

test_that("translate sql server -> Netezza DELETE FROM WHERE", {
  sql <- translate("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "delete from ACHILLES_results where analysis_id IN (1, 2, 3);")
})

test_that("translate sql server -> Netezza CAST AS VARCHAR", {
  sql <- translate("CAST(person_id AS VARCHAR);", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "CAST(person_id AS VARCHAR(1000));")
})

test_that("translate sql server -> netezza TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> netezza TOP subquery", {
  sql <- translate("SELECT * FROM (SELECT TOP 10 * FROM my_table WHERE a = b);",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "SELECT * FROM (SELECT * FROM my_table WHERE a = b LIMIT 10);")
})

test_that("translate sql server -> netezza ISNUMERIC", {
  sql <- translate("SELECT ISNUMERIC(a) FROM b", targetDialect = "netezza")
  expect_equal_ignore_spaces(
    sql,
    "SELECT CASE WHEN translate(a,'0123456789','') in ('','.','-','-.') THEN 1 ELSE 0 END FROM b"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "netezza")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE CASE WHEN translate(a,'0123456789','') in ('','.','-','-.') THEN 1 ELSE 0 END = 1"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "netezza")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE CASE WHEN translate(a,'0123456789','') in ('','.','-','-.') THEN 1 ELSE 0 END = 0"
  )
})

test_that("translate sql server -> Netezza concat with more than two arguments", {
  sql <- translate("SELECT CONCAT(a,b,c,d,e) FROM x;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT a || b || c || d || e FROM x;")
})

test_that("translate sql server -> Netezza nested concat ", {
  sql <- translate("SELECT CONCAT(CONCAT(CONCAT(a,CONCAT(b,c)),d),e) FROM x;",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "SELECT a || b || c || d || e FROM x;")
})

test_that("translate sql server -> Netezza clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "-- netezza does not support indexes")
})

test_that("translate sql server -> Netezza index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "netezza"
  )
  expect_equal_ignore_spaces(sql, "-- netezza does not support indexes")
})

test_that("translate sql server -> Netezza analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "GENERATE STATISTICS ON results_schema.heracles_results;")
})

test_that("translate sql server -> Netezza DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "DROP TABLE test IF EXISTS;")
})

test_that("translate  -> sql server DROP TABLE IF EXISTS temp", {
  sql <- translate("DROP TABLE IF EXISTS #my_temp;", targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "DROP TABLE my_temp IF EXISTS;")
})
