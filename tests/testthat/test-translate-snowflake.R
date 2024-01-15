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

test_that("translate sql server -> Snowflake select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Snowflake SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROW_NUMBER() OVER CAST(CONCAT('x', val) AS BIT(32)) rn WHERE rn <= 1"
  )
})

test_that("translate sql server -> Snowflake clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(sql, "-- snowflake does not support indexes")
})

test_that("translate sql server -> Snowflake index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(sql, "-- snowflake does not support indexes")
})

test_that("translate sql server -> Snowflake USE", {
  sql <- translate("USE vocabulary;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "USE vocabulary;")
})

test_that("translate sql server -> Snowflake INSERT INTO WITH ", {
  sql <- translate("WITH a AS (SELECT * FROM b) INSERT INTO c SELECT * FROM a;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "INSERT INTO c WITH a AS (SELECT * FROM b) SELECT * FROM a;")
})


test_that("translate sql server -> Snowflake WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1  AS (SELECT a FROM b)  SELECT\nc\nFROM\ncte1;"
  )
})

test_that("translate sql server -> Snowflake WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d AS\nSELECT\nc ;"
  )
})

test_that("translate sql server -> Snowflake location reserved word", {
  sql <- translate("select count(1) from omop_cdm.location;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "select count(1) from omop_cdm.location;")
})

test_that("translate sql server -> Snowflake TOP in subqueries", {
  sql <- translate("select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003)",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "select statistic_value from achilles_results join (SELECT  count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003)"
  )
})

test_that("translate sql server -> Snowflake CREATE TABLE with NOT NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 REAL NOT NULL, c9 SMALLINT NOT NULL, c10 STRING NOT NULL, c11 TIMESTAMP NOT NULL, c12 TINYINT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 DATETIME NOT NULL, c16 INTEGER NOT NULL)",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 REAL NOT NULL, c9 SMALLINT NOT NULL, c10 STRING NOT NULL, c11 TIMESTAMP NOT NULL, c12 TINYINT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 TIMESTAMP NOT NULL, c16 INTEGER NOT NULL)"
  )
})

test_that("translate sql server -> Snowflake CREATE TABLE with NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 REAL NULL, c9 SMALLINT NULL, c10 STRING NULL, c11 TIMESTAMP NULL, c12 TINYINT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 DATETIME NULL)",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 REAL NULL, c9 SMALLINT NULL, c10 STRING NULL, c11 TIMESTAMP NULL, c12 TINYINT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 TIMESTAMP NULL)"
  )
})

test_that("translate sql server -> Snowflake clause with NOT NULL", {
  sql <- translate("SELECT * FROM x WHERE y IS NOT NULL", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT * FROM x WHERE y IS NOT NULL")
})

test_that("translate sql server -> Snowflake CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 TIMESTAMP CONSTRAINT a_c1_def DEFAULT NOW())",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP CONSTRAINT a_c1_def DEFAULT NOW())")
})

test_that("translate sql server -> Snowflake CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 TIMESTAMP DEFAULT NOW())", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP DEFAULT NOW())")
})

test_that("translate sql server -> Snowflake DATEFROMPARTS()", {
  sql <- translate("SELECT DATEFROMPARTS('1977', '10', '12')", targetDialect = "snowflake")
  expect_equal_ignore_spaces(
    sql,
    "SELECT TO_DATE(TO_CHAR('1977','FM0000')||'-'||TO_CHAR('10','FM00')||'-'||TO_CHAR('12','FM00'), 'YYYY-MM-DD')"
  )
})

test_that("translate sql server -> Snowflake EOMONTH()", {
  sql <- translate("SELECT eomonth(payer_plan_period_start_date) AS obs_month_end",
    targetDialect = "snowflake"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT last_day(payer_plan_period_start_date) AS obs_month_end"
  )
})

test_that("translate sql server -> Snowflake ISNUMERIC", {
  sql <- translate("SELECT ISNUMERIC(a) FROM b", targetDialect = "snowflake")
  expect_equal_ignore_spaces(
    sql,
    "SELECT IS_REAL(TRY_TO_NUMERIC(a)) FROM b"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "snowflake")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE IS_REAL(TRY_TO_NUMERIC(a)) = 1"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "snowflake")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE IS_REAL(TRY_TO_NUMERIC(a)) = 0"
  )
})

test_that("translate sql server -> Snowflake CEILING", {
  sql <- translate("SELECT CEILING(0.1);", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT CEIL(0.1);")
})

test_that("translate sql server -> Snowflake TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT  * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> snowflake drvd()", {
  sql <- translate("SELECT
      TRY_CAST(name AS VARCHAR(MAX)) AS name,
      TRY_CAST(speed AS FLOAT) AS speed
    FROM (  VALUES ('A', 1.0), ('B', 2.0)) AS drvd(name, speed);", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT\n      CAST(name AS TEXT) AS name,\n      CAST(speed AS FLOAT) AS speed\n    FROM (VALUES ('A', 1.0), ('B', 2.0)) AS values_table (name, speed);")
})

test_that("translate sql server -> snowflake ...", {
  sql <- translate("SELECT x FROM my_table...1;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT x FROM my_tablexxx1;")
})

test_that("translate sql server -> snowflake a.b...", {
  sql <- translate("SELECT x FROM a.my_table...1;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT x FROM axmy_tablexxx1;")
})

test_that("translate sql server -> snowflake a.b.c...", {
  sql <- translate("SELECT x FROM a.b.my_table...1;", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "SELECT x FROM axbxmy_tablexxx1;")
})

test_that("translate sql server -> snowflake a.b.c... in paren", {
  sql <- translate("(SELECT x FROM a.b.my_table...1)", targetDialect = "snowflake")
  expect_equal_ignore_spaces(sql, "(SELECT x FROM axbxmy_tablexxx1)")
})




# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')

