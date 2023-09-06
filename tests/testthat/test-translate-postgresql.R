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

test_that("translate sql server -> PostgreSQL USE", {
  sql <- translate("USE vocabulary;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SET search_path TO vocabulary;")
})

test_that("translate sql server -> PostgreSQL string concat", {
  sql <- translate("'x' + b ( 'x' + b)", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "'x' || b ( 'x' || b)")
})

test_that("translate sql server -> PostgreSQL string concat", {
  sql <- translate("a + ';b'", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "a || ';b'")
})

test_that("translate sql server -> PostgreSQL string concat", {
  sql <- translate("a + ';('", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "a || ';('")
})


test_that("translate sql server -> PostgreSQL add month", {
  sql <- translate("DATEADD(mm,1,date)", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "(date + 1*INTERVAL'1 month')")
})

test_that("translate sql server -> PostgreSQL date diff (month)", {
  sql <- translate("SELECT DATEDIFF(month,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(sql, "SELECT (extract(year from age(CAST(drug_era_end_date AS DATE), CAST(drug_era_start_date AS DATE)))*12 + extract(month from age(CAST(drug_era_end_date AS DATE), CAST(drug_era_start_date AS DATE))))  FROM drug_era;")
})

test_that("translate sql server -> Postgres WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Postgres WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> Postgres WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "CREATE TABLE d AS\nSELECT\nc ;")
})


test_that("translate sql server -> Postgres WITH INSERT INTO SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;"
  )
})

test_that("translate sql server -> Postgres create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> Postgres select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Postgres select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Postgres SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROW_NUMBER() OVER CAST(CONCAT('x', val) AS BIT(32)) rn WHERE rn <= 1"
  )
})

test_that("translate sql server -> PostgreSql TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> PostgreSql TOP subquery", {
  sql <- translate("SELECT name FROM (SELECT TOP 1 name FROM my_table WHERE a = b);",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT name FROM (SELECT name FROM my_table WHERE a = b LIMIT 1);"
  )
})

test_that("translate sql server -> postgres date to varchar", {
  sql <- translate("SELECT CONVERT(VARCHAR,start_date,112) FROM table;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(sql, "SELECT TO_CHAR(start_date, 'YYYYMMDD') FROM table;")
})


test_that("translate sql server -> postgresql natural log", {
  sql <- translate("SELECT LOG(number) FROM table", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((number) AS REAL)) FROM table")
})

test_that("translate sql server -> postgresql log base 10", {
  sql <- translate("SELECT LOG10(number) FROM table;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT LOG(10,CAST((number) AS NUMERIC)) FROM table;")
})

test_that("translate sql server -> postgresql log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table", targetDialect = "postgresql")
  expect_equal_ignore_spaces(
    sql,
    "SELECT LOG(CAST(( base) AS NUMERIC),CAST((number) AS NUMERIC)) FROM table"
  )
})

test_that("translate sql server -> postgres ISNUMERIC", {
  sql <- translate("SELECT CASE WHEN ISNUMERIC(a) = 1 THEN a ELSE b FROM c;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CASE WHEN CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 1 THEN a ELSE b FROM c;"
  )
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "postgresql")
  expect_equal_ignore_spaces(
    sql,
    "SELECT a FROM table WHERE CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 1"
  )
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "postgresql")
  expect_equal_ignore_spaces(
    sql,
    "SELECT a FROM table WHERE CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 0"
  )
})

test_that("Postgres String literal within CTE should be explicitly casted to character type", {
  sql <- translate("WITH expression AS(SELECT 'my literal', col1, CAST('other literal' as VARCHAR(MAX)), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(
    sql,
    "WITH  expression  AS (SELECT CAST('my literal' as TEXT), col1, CAST('other literal' as TEXT), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;"
  )
})

test_that("translate sql server -> Postgres analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
    targetDialect = "postgresql"
  )
  expect_equal_ignore_spaces(sql, "ANALYZE results_schema.heracles_results;")
})

test_that("translate sql server -> Postgres DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP, b TIMESTAMP);")
})

test_that("translate sql server -> postgresql DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test;")
})

test_that("translate sql server -> Postgres comments in quotes", {
  sql <- "WITH cte_all
AS (
SELECT * FROM my_table

UNION ALL

SELECT '(--12 hours fasting)' AS check_description
)
INSERT INTO cdm.main
SELECT *
FROM cte_all;"
  sql <- translate(sql, targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "WITH cte_all\n AS (SELECT * FROM my_table\nUNION ALL\nSELECT CAST('(--12 hours fasting)' as TEXT) AS check_description\n)\nINSERT INTO cdm.main\nSELECT *\nFROM cte_all;")
})

test_that("translate sql server -> Postgres comments in quotes", {
  sql <- "WITH cte_all
AS (
SELECT * FROM my_table

UNION ALL

SELECT '(/*12 hours fasting)' AS check_description
)
INSERT INTO cdm.main
SELECT *
FROM cte_all;"
  sql <- translate(sql, targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "WITH cte_all\n AS (SELECT * FROM my_table\nUNION ALL\nSELECT CAST('(/*12 hours fasting)' as TEXT) AS check_description\n)\nINSERT INTO cdm.main\nSELECT *\nFROM cte_all;")
})

test_that("translate sql server -> postgresql IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table ;")
})

test_that("translate sql server -> postgresql ALTER TABLE ADD single", {
  sql <- translate("ALTER TABLE my_table ADD a INT;", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "ALTER TABLE my_table  ADD COLUMN a INT;")
})

test_that("translate sql server -> postgresql ALTER TABLE ADD multiple", {
  sql <- translate("ALTER TABLE my_table ADD a INT, b INT, c VARCHAR(255);", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "ALTER TABLE my_table ADD COLUMN a INT, ADD COLUMN b INT, ADD COLUMN c VARCHAR(255);")
})
