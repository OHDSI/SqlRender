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

test_that("translate sql server -> Impala select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY fnv_hash(CAST(person_id AS varchar))) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Impala SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROW_NUMBER() OVER cast(conv(val, 16, 10) as int) rn WHERE rn <= 1"
  )
})

test_that("translate sql server -> Impala clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(sql, "-- impala does not support indexes")
})

test_that("translate sql server -> Impala index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(sql, "-- impala does not support indexes")
})

test_that("translate sql server -> Impala USE", {
  sql <- translate("USE vocabulary;", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "USE vocabulary;")
})

test_that("translate sql server -> Impala CAST(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "CASE TYPEOF('20000101' ) WHEN 'TIMESTAMP' THEN CAST('20000101'  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST('20000101'  AS STRING), 1, 4), SUBSTR(CAST('20000101'  AS STRING), 5, 2), SUBSTR(CAST('20000101'  AS STRING), 7, 2)), 'UTC') END;"
  )
})

test_that("translate sql server -> Impala DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATEDIFF(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, CASE TYPEOF(drug_era_start_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_start_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_start_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_start_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_start_date  AS STRING), 7, 2)), 'UTC') END) FROM drug_era;"
  )
})

test_that("translate sql server -> Impala DATEDIFF (MONTH)", {
  sql <- translate("SELECT DATEDIFF(month,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(sql, "SELECT INT_MONTHS_BETWEEN(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, CASE TYPEOF(drug_era_start_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_start_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_start_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_start_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_start_date  AS STRING), 7, 2)), 'UTC') END) FROM drug_era;")
})

test_that("translate sql server -> Impala DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT DATE_ADD(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, 30) FROM drug_era;"
  )
})

test_that("translate sql server -> Impala WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Impala WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d STORED AS PARQUET \nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\n c \nFROM\n cte1;\n COMPUTE STATS d;"
  )
})

test_that("translate sql server -> Impala WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d STORED AS PARQUET AS\nSELECT\n c;\n COMPUTE STATS d;"
  )
})

test_that("translate sql server -> Impala create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> Impala DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS cohort;")
})

test_that("translate sql server -> Impala UNION ORDER BY", {
  sql <- translate("(SELECT a FROM b UNION SELECT a FROM c) ORDER BY a", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "SELECT * FROM \n ( SELECT a FROM b \n UNION \n SELECT a FROM c ) \n AS t1 ORDER BY a"
  )
})

test_that("translate sql server -> Impala RIGHT functions", {
  sql <- translate("SELECT RIGHT(x,4);", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4);")
})

test_that("translate sql server -> Impala DELETE FROM", {
  sql <- translate("delete from ACHILLES_results;", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "TRUNCATE TABLE ACHILLES_results;")
})

test_that("translate sql server -> Impala DELETE FROM WHERE", {
  sql <- translate("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT OVERWRITE TABLE ACHILLES_results SELECT * FROM ACHILLES_results WHERE NOT(analysis_id IN (1, 2, 3));"
  )
})

test_that("translate sql server -> Impala location reserved word", {
  sql <- translate("select count(1) from omop_cdm.location;", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "select count(1) from omop_cdm.`location`;")
})

test_that("translate sql server -> Impala TOP in subqueries", {
  sql <- translate("select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003)",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "select statistic_value from achilles_results join (SELECT count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003)"
  )
})

test_that("translate sql server -> Impala CREATE TABLE with NOT NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 REAL NOT NULL, c9 SMALLINT NOT NULL, c10 STRING NOT NULL, c11 TIMESTAMP NOT NULL, c12 TINYINT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 DATETIME NOT NULL, c16 INTEGER NOT NULL)",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a (c1 BIGINT, c2 BOOLEAN, c3 CHAR(1), c4 DECIMAL, c5 DOUBLE, c6 FLOAT, c7 INT, c8 REAL, c9 SMALLINT, c10 STRING, c11 TIMESTAMP, c12 TINYINT, c13 VARCHAR(10), c14 TIMESTAMP, c15 TIMESTAMP, c16 INT)"
  )
})

test_that("translate sql server -> Impala CREATE TABLE with NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 REAL NULL, c9 SMALLINT NULL, c10 STRING NULL, c11 TIMESTAMP NULL, c12 TINYINT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 DATETIME NULL)",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE a (c1 BIGINT, c2 BOOLEAN, c3 CHAR(1), c4 DECIMAL, c5 DOUBLE, c6 FLOAT, c7 INT, c8 REAL, c9 SMALLINT, c10 STRING, c11 TIMESTAMP, c12 TINYINT, c13 VARCHAR(10), c14 TIMESTAMP, c15 TIMESTAMP)"
  )
})

test_that("translate sql server -> Impala clause with NOT NULL", {
  sql <- translate("SELECT * FROM x WHERE y IS NOT NULL", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT * FROM x WHERE y IS NOT NULL")
})

test_that("translate sql server -> Impala CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 TIMESTAMP CONSTRAINT a_c1_def DEFAULT NOW())",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translate sql server -> Impala CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 TIMESTAMP DEFAULT NOW())", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translate sql server -> Impala stats reserved word", {
  sql <- translate("SELECT * FROM strata_stats AS stats", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT * FROM strata_stats AS _stats")
})

test_that("translate sql server -> Impala DATEFROMPARTS()", {
  sql <- translate("SELECT DATEFROMPARTS('1977', '10', '12')", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "SELECT to_timestamp(CONCAT(CAST('1977' AS VARCHAR),'-',CAST('10' AS VARCHAR),'-',CAST('12' AS VARCHAR)), 'yyyy-M-d')"
  )
})

test_that("translate sql server -> Impala EOMONTH()", {
  sql <- translate("SELECT eomonth(payer_plan_period_start_date) AS obs_month_end",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT days_sub(add_months(trunc(CAST(payer_plan_period_start_date AS TIMESTAMP), 'MM'),1),1) AS obs_month_end"
  )
})

test_that("translate sql server -> Impala ISNUMERIC", {
  sql <- translate("SELECT ISNUMERIC(a) FROM b", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "SELECT case when regexp_like(a,'^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') then 1 else 0 end FROM b"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE case when regexp_like(a,'^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') then 1 else 0 end = 1"
  )
  sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "impala")
  expect_equal_ignore_spaces(
    sql,
    "SELECT some FROM table WHERE case when regexp_like(a,'^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') then 1 else 0 end = 0"
  )
})

test_that("translate sql server -> Impala data types", {
  sql <- translate("CREATE TABLE a (c1 DOUBLE PRECISION)", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 DOUBLE)")
})

test_that("translate sql server -> Impala escape chars", {
  sql <- translate("INSERT INTO t VALUES('some \"string\" ''with escape'' chars')",
    targetDialect = "impala"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT INTO t VALUES(CONCAT('some \\042string\\042 ','\\047','with escape','\\047','chars'))"
  )
})

test_that("translate sql server -> impala TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> Impala analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "COMPUTE STATS results_schema.heracles_results;")
})

test_that("translate sql server -> impala temp table field ref", {
  sql <- translate("SELECT #tmp.name FROM #tmp;", targetDialect = "impala", tempEmulationSchema = "ts")
  expect_equal_ignore_spaces(sql, sprintf("SELECT %stmp.name FROM ts.%stmp;", getTempTablePrefix(), getTempTablePrefix()))
})

# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')
