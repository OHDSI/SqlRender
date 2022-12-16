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

test_that("translate sql server -> HIVE DATEDIFF(MONTH)", {
  sql <- translate("SELECT DATEDIFF(Month,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "SELECT CAST(MONTHS_BETWEEN(CAST(drug_era_end_date AS TIMESTAMP ), CAST(drug_era_start_date AS TIMESTAMP )) AS INT) FROM drug_era;")
})

test_that("translate sql server -> Hive clustered index is not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "-- hive does not support indexes")
})

test_that("translate sql server -> Hive index is not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "-- hive does not support indexes")
})

test_that("translate sql server -> Hive index with Where is not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date) WHERE cohort_definition_id=1;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "-- hive does not support indexes")
})

test_that("translate sql server -> Hive CHARINDEX from position", {
  sql <- translate("SELECT CHARINDEX('test','abctest') FROM table", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT INSTR('abctest','test') FROM table")
})

test_that("translate sql server -> Hive COUNT", {
  sql <- translate("SELECT COUNT_BIG('test') FROM table", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT COUNT('test') FROM table")
})

test_that("translate sql server -> Hive left SUBSTR", {
  sql <- translate("SELECT LEFT('test',3)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR('test',1,3)")
})

test_that("translate sql server -> Hive right SUBSTR", {
  sql <- translate("SELECT RIGHT('test',3)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR('test',-3)")
})

test_that("translate sql server -> Hive LENGTH", {
  sql <- translate("SELECT LEN('test')", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT LENGTH('test')")
})

test_that("translate sql server -> Hive LN", {
  sql <- translate("SELECT LOG(10)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT LN(10)")
})

test_that("translate sql server -> Hive new ID", {
  sql <- translate("SELECT NEWID()", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT reflect('java.util.UUID','randomUUID')")
})

test_that("translate sql server -> Hive ROUND", {
  sql <- translate("SELECT ROUND('100.2564', 2)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT ROUND(CAST('100.2564' AS DOUBLE),2)")
})

test_that("translate sql server -> Hive SQUARE", {
  sql <- translate("SELECT SQUARE(2)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT ((2)*(2))")
})

test_that("translate sql server -> Hive STDDEV", {
  sql <- translate("SELECT STDEV(4)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT STDDEV_POP(4)")
})

test_that("translate sql server -> Hive VARIANCE", {
  sql <- translate("SELECT VAR(4)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT VARIANCE(4)")
})

test_that("translate sql server -> Hive DATE_ADD day", {
  sql <- translate("SELECT DATEADD(d,30,CAST(drug_era_end_date AS DATE)) FROM drug_era;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "SELECT DATE_ADD(drug_era_end_date, 30) FROM drug_era;")
})

test_that("translate sql server -> Hive DATE_ADD month", {
  sql <- translate("SELECT DATEADD(month,3,CAST(drug_era_end_date AS DATE)) FROM drug_era;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(ADD_MONTHS(drug_era_end_date, 3) AS TIMESTAMP) FROM drug_era;"
  )
})

test_that("translate sql server -> Hive DATEFROMPARTS", {
  sql <- translate("SELECT DATEFROMPARTS(1999,12,12);", targetDialect = "hive")
  expect_equal_ignore_spaces(
    sql,
    "SELECT CAST(CONCAT(CAST(1999 AS STRING),'-',CAST(12 AS STRING),'-',CAST(12 AS STRING)) AS TIMESTAMP);"
  )
})

test_that("translate sql server -> Hive EOMONTH", {
  sql <- translate("SELECT eomonth(drug_era_end_date);", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT CAST(last_day(drug_era_end_date) AS TIMESTAMP);")
})

test_that("translate sql server -> Hive TIMESTAMP", {
  sql <- translate("SELECT GETDATE();", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT unix_timestamp();")
})

test_that("translate sql server -> Hive Year TIMESTAMP", {
  sql <- translate("SELECT year(unix_timestamp());", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SELECT year(from_unixtime(unix_timestamp()));")
})

test_that("translate sql server -> Hive CREATE TABLE", {
  sql <- translate("IF OBJECT_ID('test.testing', 'U') IS NULL CREATE TABLE test.testing (id int);",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS test.testing (id int);")
})

test_that("translate sql server -> Hive DROP TABLE", {
  sql <- translate("IF OBJECT_ID('test.testing', 'U') IS NOT NULL DROP TABLE test.testing;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test.testing;")
})

test_that("translate sql server -> Hive UNION", {
  sql <- translate("(SELECT test UNION SELECT ytest) ORDER BY", targetDialect = "hive")
  expect_equal_ignore_spaces(
    sql,
    "SELECT * FROM\n(SELECT test\nUNION\nSELECT ytest)\nAS t1 ORDER BY"
  )
})

test_that("translate sql server -> Hive PARTITION IF NOT EXISTS", {
  sql <- translate("HINT PARTITION(cohort_definition_id)
IF OBJECT_ID('@results_schema.heracles_results_dist', 'U') IS NULL
CREATE TABLE heracles_results_dist
(
cohort_definition_id int,
analysis_id int,
stratum_1 varchar(255),
);", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "partitioned table
CREATE TABLE IF NOT EXISTS heracles_results_dist
(   analysis_id int,
stratum_1 varchar(255),
)
PARTITIONED BY(cohort_definition_id);")
})

test_that("translate sql server -> Hive PARTITION", {
  sql <- translate("HINT PARTITION(cohort_definition_id)
CREATE TABLE heracles_results_dist
(
cohort_definition_id int,
analysis_id int,
stratum_1 varchar(255),
);", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "partitioned table
CREATE TABLE heracles_results_dist
(   analysis_id int,
stratum_1 varchar(255),
)
PARTITIONED BY(cohort_definition_id);")
})

test_that("translate sql server -> Hive BUCKET IF NOT EXISTS", {
  sql <- translate("HINT BUCKET(analysis_id, 64)
IF OBJECT_ID('@results_schema.heracles_results_dist', 'U') IS NULL
CREATE TABLE heracles_results_dist
(
cohort_definition_id int,
analysis_id int,
stratum_1 varchar(255),
);", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "table with bucket
CREATE TABLE IF NOT EXISTS heracles_results_dist
(cohort_definition_id int,
analysis_id int,
stratum_1 varchar(255),
)
CLUSTERED by(analysis_id) into 64 BUCKETS;")
})

test_that("translate sql server -> Hive BUCKET", {
  sql <- translate("HINT BUCKET(analysis_id, 64)
CREATE TABLE heracles_results_dist
(
cohort_definition_id int,
analysis_id int,
stratum_1 varchar(255),
);", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "table with bucket
CREATE TABLE heracles_results_dist
(cohort_definition_id int,
analysis_id int,
stratum_1 varchar(255),
)
CLUSTERED by(analysis_id) into 64 BUCKETS;")
})

test_that("translate sql server -> Hive dbo", {
  sql <- translate(".dbo.", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, ".")
})

test_that("translate sql server -> Hive TOP in subqueries", {
  sql <- translate("select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003)",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(
    sql,
    "select statistic_value from achilles_results join (SELECT count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003)"
  )
})

test_that("translate sql server -> Hive TOP in subqueries with parentheses", {
  sql <- translate("(select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003))",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(
    sql,
    "(select statistic_value from achilles_results join (SELECT count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003))"
  )
})

test_that("translate sql server -> Hive DATE", {
  sql <- translate("DATE", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive DATETIME", {
  sql <- translate("DATETIME", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive DATETIME2", {
  sql <- translate("DATETIME2", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive BIGINT NOT NULL", {
  sql <- translate("BIGINT NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "BIGINT")
})

test_that("translate sql server -> Hive BOOLEAN NOT NULL", {
  sql <- translate("BOOLEAN NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "BOOLEAN")
})

test_that("translate sql server -> Hive CHAR NOT NULL", {
  sql <- translate("CHAR NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CHAR")
})

test_that("translate sql server -> Hive DECIMAL NOT NULL", {
  sql <- translate("DECIMAL NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "DECIMAL")
})

test_that("translate sql server -> Hive DOUBLE NOT NULL", {
  sql <- translate("DOUBLE NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "DOUBLE")
})

test_that("translate sql server -> Hive FLOAT NOT NULL", {
  sql <- translate("FLOAT NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive INT NOT NULL", {
  sql <- translate("INT NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "INT")
})

test_that("translate sql server -> Hive REAL NOT NULL", {
  sql <- translate("REAL NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive SMALLINT NOT NULL", {
  sql <- translate("SMALLINT NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SMALLINT")
})

test_that("translate sql server -> Hive STRING NOT NULL", {
  sql <- translate("STRING NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "VARCHAR")
})

test_that("translate sql server -> Hive TIMESTAMP NOT NULL", {
  sql <- translate("TIMESTAMP NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive TINYINT NOT NULL", {
  sql <- translate("TINYINT NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TINYINT")
})

test_that("translate sql server -> Hive VARCHAR NOT NULL", {
  sql <- translate("VARCHAR(10) NOT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "VARCHAR(10)")
})

test_that("translate sql server -> Hive BIGINT NULL", {
  sql <- translate("BIGINT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "BIGINT")
})

test_that("translate sql server -> Hive BOOLEAN NULL", {
  sql <- translate("BOOLEAN NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "BOOLEAN")
})

test_that("translate sql server -> Hive CHAR NULL", {
  sql <- translate("CHAR NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CHAR")
})

test_that("translate sql server -> Hive DECIMAL NULL", {
  sql <- translate("DECIMAL NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "DECIMAL")
})

test_that("translate sql server -> Hive DOUBLE NULL", {
  sql <- translate("DOUBLE NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "DOUBLE")
})

test_that("translate sql server -> Hive FLOAT NULL", {
  sql <- translate("FLOAT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive INT NULL", {
  sql <- translate("INT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "INT")
})

test_that("translate sql server -> Hive REAL NULL", {
  sql <- translate("FLOAT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive SMALLINT NULL", {
  sql <- translate("SMALLINT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "SMALLINT")
})

test_that("translate sql server -> Hive STRING NULL", {
  sql <- translate("STRING NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "VARCHAR")
})

test_that("translate sql server -> Hive TIMESTAMP NULL", {
  sql <- translate("TIMESTAMP NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive TINYINT NULL", {
  sql <- translate("TINYINT NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "TINYINT")
})

test_that("translate sql server -> Hive VARCHAR NULL", {
  sql <- translate("VARCHAR(10) NULL", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "VARCHAR(10)")
})

test_that("translate sql server -> Hive CHAR", {
  sql <- translate("CHAR,", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CHAR(1),")
})

test_that("translate sql server -> Hive CHAR\n", {
  sql <- translate("CHAR\n+", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CHAR(1)\n")
})

test_that("translate sql server -> Hive CHAR)", {
  sql <- translate("CHAR)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CHAR(1))")
})

test_that("translate sql server -> Hive CONSTRAINT DEFAULT timestamp", {
  sql <- translate("CONSTRAINT test DEFAULT unix_timestamp()", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Hive DEFAULT timestamp", {
  sql <- translate("DEFAULT unix_timestamp()", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Hive UPDATE STATISTICS", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "-- hive does not support COMPUTE STATS")
})

test_that("translate sql server -> Hive CAST VARCHAR", {
  sql <- translate("CAST(10 AS VARCHAR)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CAST(10 AS VARCHAR(1000))")
})

test_that("translate sql server -> Hive COALESCE", {
  sql <- translate("ISNULL(abc,gde)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "COALESCE(abc,gde)")
})

test_that("translate sql server -> Hive WITH AS temp", {
  sql <- translate("WITH cteRawData as (select coh_id FROM #raw_706),
overallStats as (select coh_id from cteRawData),
valueStats as (select total FROM (select coh_id FROM cteRawData GROUP BY coh_id) D)
select o.coh_id, 706 as analysis_id into #results_dist_706 from valueStats s
join overallStats o on s.coh_id = o.coh_id;", targetDialect = "hive")
  expect_equal_ignore_spaces(
    sql,
    "DROP TABLE IF EXISTS cteRawData; DROP TABLE IF EXISTS overallStats; DROP TABLE IF EXISTS valueStats;
CREATE TEMPORARY TABLE cteRawData AS select coh_id FROM raw_706;
CREATE TEMPORARY TABLE overallStats AS select coh_id from cteRawData;
CREATE TEMPORARY TABLE valueStats AS select total FROM (select coh_id FROM cteRawData GROUP BY coh_id) D;
CREATE TEMPORARY TABLE results_dist_706 AS SELECT o.coh_id, 706 as analysis_id FROM valueStats s
join overallStats o on s.coh_id = o.coh_id;"
  )
})

test_that("translate sql server -> Hive TEMP TABLE", {
  sql <- translate("select coh_id into #raw_706 from cteRawData;", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CREATE TEMPORARY TABLE IF NOT EXISTS raw_706 AS
SELECT
coh_id
FROM
cteRawData;")
})

test_that("translate sql server -> Hive TEMP TABLE without from", {
  sql <- translate("select coh_id into #raw_706;", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CREATE TEMPORARY TABLE IF NOT EXISTS raw_706 AS
SELECT
coh_id;")
})

test_that("translate sql server -> Hive TEMP TABLE if not exists", {
  sql <- translate("CREATE TABLE #raw_706 (coh_id int)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "CREATE TEMPORARY TABLE IF NOT EXISTS raw_706 (coh_id int)")
})

test_that("translate sql server -> Hive several TEMP TABLE", {
  sql <- translate("CREATE TEMPORARY TABLE raw_707 as (select coh_id FROM #raw_706), overallStats (coh_id) as (select coh_id from cteRawData)
;", targetDialect = "hive")
  expect_equal_ignore_spaces(
    sql,
    "DROP TABLE IF EXISTS raw_707; DROP TABLE IF EXISTS overallStats; CREATE TEMPORARY TABLE raw_707 AS (select coh_id FROM raw_706)
;
CREATE TEMPORARY TABLE overallStats AS (select coh_id from cteRawData)
;"
  )
})

test_that("translate sql server -> Hive several TEMP TABLE without definitions", {
  sql <- translate("CREATE TEMPORARY TABLE raw_707 as (select coh_id FROM #raw_706), overallStats as (select coh_id from cteRawData)
;", targetDialect = "hive")
  expect_equal_ignore_spaces(
    sql,
    "DROP TABLE IF EXISTS raw_707; DROP TABLE IF EXISTS overallStats; CREATE TEMPORARY TABLE raw_707 AS (select coh_id FROM raw_706)
;
CREATE TEMPORARY TABLE overallStats AS (select coh_id from cteRawData)
;"
  )
})

test_that("translate sql server -> Hive DROP with definition", {
  sql <- translate("DROP TABLE IF EXISTS test.testing (id int)", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test.testing ")
})

test_that("translate sql server -> Hive Subquery", {
  sql <- translate("SELECT o.coh_id, 706 as analysis_id into results_dist_706 from valueStats;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS results_dist_706 AS
SELECT
o.coh_id, 706 as analysis_id
FROM
valueStats;")
})

test_that("translate sql server -> Hive DISTINCT", {
  sql <- translate("SELECT o.coh_id, 706 as analysis_id into results_dist_706 from valueStats;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS results_dist_706 AS
SELECT
o.coh_id, 706 as analysis_id
FROM
valueStats;")
})

test_that("translate sql server -> Hive intersect distinct", {
  sql <- translate("SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s;",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2;"
  )
})

test_that("translate sql server -> Hive bracketed intersect distinct", {
  sql <- translate("(SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s)",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(
    sql,
    "(SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2)"
  )
})

test_that("translate sql server -> Hive Dash", {
  sql <- translate("#", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Hive extra space", {
  sql <- translate("(coh_id int, analysis_id int)  AS select o.coh_id, 706 as analysis_id FROM valueStats s",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(
    sql,
    "(coh_id int, analysis_id int) AS select o.coh_id, 706 as analysis_id FROM valueStats s"
  )
})

test_that("translate sql server -> Hive table without definition", {
  sql <- translate("CREATE TABLE cteRawData (coh_id int) AS select coh_id FROM raw_706",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE cteRawData AS select coh_id FROM raw_706")
})

test_that("translate sql server -> Hive digits", {
  sql <- translate("WHEN .123456 * ", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "WHEN 0.123456 * ")
})

test_that("translate sql server -> Hive digits", {
  sql <- translate("WHEN .123456 * ", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "WHEN 0.123456 * ")
})

test_that("translate sql server -> Hive ISNUMERIC", {
  sql <- translate("select ISNUMERIC(a) from b", targetDialect = "hive")
  expect_equal_ignore_spaces(
    sql,
    "select case when cast(a as double) is not null then 1 else 0 end from b"
  )
})

test_that("translate sql server -> Hive AS", {
  sql <- translate("as \"test_variable\"", targetDialect = "hive")
  expect_equal_ignore_spaces(sql, "as test_variable")
})

test_that("translate sql server -> Hive HASHBYTES", {
  sql <- translate("SELECT AVG(CAST(CAST(CONVERT(VARBINARY, HASHBYTES('MD5',line), 1) AS INT) AS BIGINT)) as checksum",
    targetDialect = "hive"
  )
  expect_equal_ignore_spaces(sql, "SELECT AVG(CAST(CAST(hash(line) AS INT) AS BIGINT)) as checksum")
})
