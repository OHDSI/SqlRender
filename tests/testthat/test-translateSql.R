library("testthat")

# For debugging: force reload of patterns:
# rJava::J("org.ohdsi.sql.SqlTranslate")$setReplacementPatterns("inst/csv/replacementPatterns.csv")

expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equal(string1, string2)
}

expect_match_ignore_spaces <- function(string1, regexp) {
  string1 <- gsub(" +", " ", string1)
  expect_match(string1, regexp)
}

test_that("translate sql server -> Oracle DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translate sql server -> Oracle DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT (drug_era_end_date + NUMTODSINTERVAL(30, 'day')) FROM drug_era;")
})

test_that("translate sql server -> Oracle functional index", {
    sql <- translate("CREATE INDEX name1 ON someTable (firstColumn,secondColumn) WHERE someCondition;", 
                            targetDialect = "oracle")
    expect_equal_ignore_spaces(sql, "CREATE INDEX name1 ON someTable (CASE WHEN someCondition THEN firstColumn END, CASE WHEN someCondition THEN secondColumn END);")
})

test_that("translate sql server -> Oracle USE", {
  sql <- translate("USE vocabulary;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "ALTER SESSION SET current_schema = vocabulary;")
})

test_that("translate sql server -> Oracle DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "BEGIN\n EXECUTE IMMEDIATE 'TRUNCATE TABLE cohort';\n EXECUTE IMMEDIATE 'DROP TABLE cohort';\nEXCEPTION\n WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;")
})


test_that("translate sql server -> Oracle CAST(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "CAST('20000101' AS DATE);")
})

test_that("translate sql server -> Oracle CONVERT(AS DATE)", {
  sql <- translate("CONVERT(DATE, '20000101');",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101', 'YYYYMMDD');")
})

test_that("translate sql server -> Oracle concatenate string operator", {
  sql <- translate("select distinct CONVERT(DATE, cast(YEAR(observation_period_start_date) as varchar(4)) + '01' + '01') as obs_year from observation_period;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "SELECT distinct TO_DATE(cast(EXTRACT(YEAR FROM observation_period_start_date) as varchar(4)) || '01' || '01', 'YYYYMMDD') as obs_year  FROM observation_period ;")
})

test_that("translate sql server -> Oracle RIGHT functions", {
  sql <- translate("select RIGHT(x,4);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4) FROM DUAL;")
})

test_that("translate sql server -> Oracle complex query", {
  sql <- translate("select CONVERT(DATE,CAST(YEAR(DATEFROMPARTS(2000,1,1)) AS VARCHAR(12)) + RIGHT('0'+MONTH(DATEFROMPARTS(2000,1,1)),2) + '01') as X;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "SELECT TO_DATE(CAST(EXTRACT(YEAR FROM TO_DATE(TO_CHAR(2000,'0000')||'-'||TO_CHAR(1,'00')||'-'||TO_CHAR(1,'00'), 'YYYY-MM-DD'))  AS varchar(12)) || SUBSTR('0' ||EXTRACT(MONTH FROM TO_DATE(TO_CHAR(2000,'0000')||'-'||TO_CHAR(1,'00')||'-'||TO_CHAR(1,'00'), 'YYYY-MM-DD')),-2) || '01', 'YYYYMMDD') as X FROM DUAL;")
})

test_that("translate sql server -> Oracle '+' in quote", {
  sql <- translate("select '+';", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT '+' FROM DUAL;")
})

test_that("translate sql server -> Oracle union in dual", {
  sql <- translate("select 1 union 2;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT 1 FROM DUAL UNION 2 FROM DUAL;")
})

test_that("translate sql server -> Oracle table alias", {
  sql <- translate("SELECT a FROM a AS a1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a FROM a a1;")
  sql <- translate("SELECT a, b FROM a AS a1 JOIN b AS b1 ON a = b WHERE c = 1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a, b FROM a a1 JOIN b b1 ON a = b WHERE c = 1;")
  sql <- translate("SELECT a, b FROM a as a1 INNER JOIN b AS b1 ON a = b LEFT JOIN c AS c1 ON b = c WHERE c IN (1,2,4);", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a, b FROM a a1 INNER JOIN b b1 ON a = b LEFT JOIN c c1 ON b = c WHERE c IN (1,2,4);")
  sql <- translate("SELECT a, b, d FROM a AS a1, b AS b1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a, b, d FROM a a1, b b1;")
  sql <- translate("SELECT a, b, d FROM a AS a1, b AS b1, c AS c1 WHERE c = 1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a, b, d FROM a a1, b b1, c c1 WHERE c = 1;")
  sql <- translate("SELECT a, b, d FROM a AS a1,(SELECT c AS c1 FROM b AS b1) AS d1 WHERE c = 1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a, b, d FROM a a1,(SELECT c AS c1 FROM b b1) d1 WHERE c = 1;")
})


test_that("translate sql server -> PostgreSQL USE", {
  sql <- translate("USE vocabulary;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SET search_path TO vocabulary;")
})

test_that("translate sql server -> PostgreSQL string concat", {
  sql <- translate("'x' + b ( 'x' + b)",
                      targetDialect = "postgresql")
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
  sql <- translate("DATEADD(mm,1,date)",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "(date + 1*INTERVAL'1 month')")
})

test_that("translate sql server -> Oracle multiple inserts in one statement", {
  sql <- translate("INSERT INTO my_table (key,value) VALUES (1,0),(2,0),(3,1)",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "INSERT ALL\nINTO my_table   (key,value) VALUES (1,0)\n INTO my_table  (key,value) VALUES (2,0)\n)\n INTO my_table   (key,value) VALUES (3,1)\nSELECT * FROM dual")
})

test_that("translate sql server -> RedShift VARCHAR(MAX)", {
  sql <- translate("VARCHAR(MAX)", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "VARCHAR(MAX)")
})

test_that("translate sql server -> Postgres WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Postgres WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translate sql server -> Postgres WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "CREATE TABLE d AS\nSELECT\nc ;")
})


test_that("translate sql server -> Postgres WITH INSERT INTO SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;")
})

test_that("translate sql server -> Oracle WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Oracle WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translate sql server -> Oracle WITH INSERT INTO SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "INSERT INTO c (d int)  WITH cte1 AS (SELECT a FROM b)  SELECT e FROM cte1;")
})

test_that("translate sql server -> PDW WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE d WITH (DISTRIBUTION = REPLICATE)\nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translate sql server -> PDW WITH SELECT INTO temp table", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO #d FROM cte1;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #d   WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translate sql server -> PDW create temp table", {
  sql <- translate("CREATE TABLE #a (x int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a (x int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE);")
})

test_that("translate sql server -> PDW create temp table with person_id", {
  sql <- translate("CREATE TABLE #a (person_id int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( person_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(person_id));")
})

test_that("translate sql server -> PDW create temp table with subject_id", {
  sql <- translate("CREATE TABLE #a (subject_id int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( subject_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(subject_id));")
})

test_that("translate sql server -> PDW create temp table with analysis_id", {
  sql <- translate("CREATE TABLE #a (analysis_id int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( analysis_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(analysis_id));")
})

test_that("translate sql server -> PDW create permanent table", {
  sql <- translate("CREATE TABLE a (x int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (x int)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translate sql server -> PDW create permanent table with person_id", {
  sql <- translate("CREATE TABLE a (person_id int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( person_id int)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translate sql server -> PDW create permanent table with subject_id", {
  sql <- translate("CREATE TABLE a (subject_id int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( subject_id int)\nWITH (DISTRIBUTION = HASH(subject_id));")
})

test_that("translate sql server -> PDW create permanent table with analysis_id", {
  sql <- translate("CREATE TABLE a (analysis_id int);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( analysis_id int)\nWITH (DISTRIBUTION = HASH(analysis_id));")
})

test_that("translate sql server -> PDW select into permanent table", {
  sql <- translate("SELECT a INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n a \nFROM\n c WHERE a = 1;")
})

test_that("translate sql server -> PDW select into permanent table with person_id", {
  sql <- translate("SELECT a, person_id, b INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = HASH(person_id))\nAS\nSELECT\n a, person_id, b \nFROM\n c WHERE a = 1;")
})

test_that("translate sql server -> PDW select into permanent table with analysis_id", {
  sql <- translate("SELECT a, analysis_id, b INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = HASH(analysis_id))\nAS\nSELECT\n a, analysis_id, b \nFROM\n c WHERE a = 1;")
})

test_that("translate sql server -> PDW CREATE TABLE with CONSTRAINT DEFAULT", {
    sql <- translate("CREATE TABLE a(c1 DATETIME CONSTRAINT a_c1_def DEFAULT GETDATE());",
                    targetDialect = "pdw")
    expect_equal_ignore_spaces(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (c1 DATETIME)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translate sql server -> PDW CREATE TABLE with CONSTRAINT DEFAULT", {
    sql <- translate("CREATE TABLE a(c1 DATETIME DEFAULT GETDATE());",
                    targetDialect = "pdw")
    expect_equal_ignore_spaces(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (c1 DATETIME)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translate sql server -> Postgres create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> Redshift create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE  IF NOT EXISTS  cohort\n  (cohort_definition_id INT)\nDISTSTYLE ALL;")
})

test_that("translate sql server -> PDW CREATE INDEX with WHERE", {
    sql <- translate("CREATE INDEX idx_a ON a(c1, c2) WHERE c3 <> '';",
    targetDialect = "pdw")
    expect_equal_ignore_spaces(sql, "CREATE INDEX idx_a ON a(c1, c2);")
})

test_that("translate sql server -> Oracle create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "BEGIN\n EXECUTE IMMEDIATE 'CREATE TABLE cohort\n (cohort_definition_id INT)';\nEXCEPTION\n WHEN OTHERS THEN\n IF SQLCODE != -955 THEN\n RAISE;\n END IF;\nEND;")
})

test_that("translate sql server -> Oracle datefromparts", {
  sql <- translate("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), 'YYYY-MM-DD') FROM table")
})

test_that("translate sql server -> redshift datefromparts", {
  sql <- translate("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql,
                             "SELECT TO_DATE(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM'), 'YYYY-MM-DD') FROM table")
})


test_that("translate sql server -> Oracle datetime to timestamp", {
  sql <- translate("CREATE TABLE x (a DATETIME)",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP)")
})

test_that("translate sql server -> Oracle select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_RANDOM.VALUE) AS rn FROM table ) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Postgres select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Redshift select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Oracle select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_CRYPTO.HASH(TO_CHAR(person_id ),2)) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Postgres select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Redshift select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Big Query select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql,
    "select column from (select column, row_number() over (order by md5(cast(person_id as STRING))) tmp where rn <= 1")
})

test_that("translate sql server -> Impala select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
      targetDialect = "impala")
  expect_equal_ignore_spaces(sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY fnv_hash(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Netezza select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY hash(CAST(person_id AS VARCHAR(1000)))) tmp WHERE rn <= 1")
})

test_that("translate sql server -> Oracle SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER TO_NUMBER(val, RPAD('X', LENGTH(val), 'X')) rn WHERE rn <= 1")
})

test_that("translate sql server -> Postgres SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER CAST(CONCAT('x', val) AS BIT(32)) rn WHERE rn <= 1")
})

test_that("translate sql server -> RedShift SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER STRTOL(LEFT(val, 15), 16) rn WHERE rn <= 1")
})

test_that("translate sql server -> Impala SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER cast(conv(val, 16, 10) as int) rn WHERE rn <= 1")
})

test_that("translate sql server -> Netezza SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER hex_to_binary(val) rn WHERE rn <= 1")
})

test_that("translate sql server -> BigQuery SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select row_number() over safe_cast(concat('0x', val) as int64) rn where rn <= 1")
})

test_that("translate sql server -> PDW cte with preceding 'with' in quotes", {
  sql <- translate("insert into x (a) values ('with'); with cte (a) as(select a from b) select a INTO #c from cte;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql,
                             "insert into x (a) values ('with'); IF XACT_STATE() = 1 COMMIT; CREATE TABLE #c WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte (a) AS (select a from b) SELECT\n a \nFROM\n cte;")
})

test_that("translate sql server throws error when invalid target is given", {
  expect_error(translate("iSELECT * FROM a;", targetDialect = "pwd"))
})


test_that("translate select into issue for pdw", {
  sql <- "SELECT @c1 INTO table FROM @c2 WHERE a = 1;"
  sql <- translate(sql, targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE table WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n @c1 \nFROM\n @c2 WHERE a = 1;")
})



test_that("translate ## issue on oracle", {
  sql <- "SELECT a FROM c##blah.table;"
  sql <- translate(sql, targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a FROM c##blah.table;")
})

# Impala tests

test_that("translate sql server -> Impala clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "-- impala does not support indexes")
})

test_that("translate sql server -> Impala index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "-- impala does not support indexes")
})

test_that("translate sql server -> Impala USE", {
  sql <- translate("USE vocabulary;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "USE vocabulary;")
})

test_that("translate sql server -> Impala CAST(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CASE TYPEOF('20000101' ) WHEN 'TIMESTAMP' THEN CAST('20000101'  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST('20000101'  AS STRING), 1, 4), SUBSTR(CAST('20000101'  AS STRING), 5, 2), SUBSTR(CAST('20000101'  AS STRING), 7, 2)), 'UTC') END;")
})

test_that("translate sql server -> Impala DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, CASE TYPEOF(drug_era_start_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_start_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_start_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_start_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_start_date  AS STRING), 7, 2)), 'UTC') END) FROM drug_era;")
})

test_that("translate sql server -> Impala DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT DATE_ADD(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, 30) FROM drug_era;")
})

test_that("translate sql server -> Impala WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Impala WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d STORED AS PARQUET \nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\n c \nFROM\n cte1;\n COMPUTE STATS d;")
})

test_that("translate sql server -> Impala WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CREATE TABLE d STORED AS PARQUET AS\nSELECT\n c;\n COMPUTE STATS d;")
})

test_that("translate sql server -> Impala create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> Impala DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql,
                             "DROP TABLE IF EXISTS cohort;")
})

test_that("translate sql server -> Impala UNION ORDER BY", {
  sql <- translate("(SELECT a FROM b UNION SELECT a FROM c) ORDER BY a",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql,
                             "SELECT * FROM \n ( SELECT a FROM b \n UNION \n SELECT a FROM c ) \n AS t1 ORDER BY a")
})

test_that("translate sql server -> Impala RIGHT functions", {
  sql <- translate("SELECT RIGHT(x,4);",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4);")
})

test_that("translate sql server -> Impala DELETE FROM", {
  sql <- translate("delete from ACHILLES_results;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "TRUNCATE TABLE ACHILLES_results;")
})

test_that("translate sql server -> Impala DELETE FROM WHERE", {
  sql <- translate("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "INSERT OVERWRITE TABLE ACHILLES_results SELECT * FROM ACHILLES_results WHERE NOT(analysis_id IN (1, 2, 3));")
})

test_that("translate sql server -> Impala location reserved word", {
  sql <- translate("select count(1) from omop_cdm.location;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "select count(1) from omop_cdm.`location`;")
})

test_that("translate sql server -> Impala TOP in subqueries", {
    sql <- translate("select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003)",
                      targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "select statistic_value from achilles_results join (SELECT count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003)")
})

test_that("translate sql server -> Impala CREATE TABLE with NOT NULL", {
  sql <- translate("CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 REAL NOT NULL, c9 SMALLINT NOT NULL, c10 STRING NOT NULL, c11 TIMESTAMP NOT NULL, c12 TINYINT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 DATETIME NOT NULL, c16 INTEGER NOT NULL)",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 BIGINT, c2 BOOLEAN, c3 CHAR(1), c4 DECIMAL, c5 DOUBLE, c6 FLOAT, c7 INT, c8 REAL, c9 SMALLINT, c10 STRING, c11 TIMESTAMP, c12 TINYINT, c13 VARCHAR(10), c14 TIMESTAMP, c15 TIMESTAMP, c16 INT)")
})

test_that("translate sql server -> Impala CREATE TABLE with NULL", {
    sql <- translate("CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 REAL NULL, c9 SMALLINT NULL, c10 STRING NULL, c11 TIMESTAMP NULL, c12 TINYINT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 DATETIME NULL)",
                    targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 BIGINT, c2 BOOLEAN, c3 CHAR(1), c4 DECIMAL, c5 DOUBLE, c6 FLOAT, c7 INT, c8 REAL, c9 SMALLINT, c10 STRING, c11 TIMESTAMP, c12 TINYINT, c13 VARCHAR(10), c14 TIMESTAMP, c15 TIMESTAMP)")
})

test_that("translate sql server -> Impala clause with NOT NULL", {
  sql <- translate("SELECT * FROM x WHERE y IS NOT NULL",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT * FROM x WHERE y IS NOT NULL")
})

test_that("translate sql server -> Impala CREATE TABLE with CONSTRAINT DEFAULT", {
    sql <- translate("CREATE TABLE a(c1 TIMESTAMP CONSTRAINT a_c1_def DEFAULT NOW())",
                    targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translate sql server -> Impala CREATE TABLE with CONSTRAINT DEFAULT", {
    sql <- translate("CREATE TABLE a(c1 TIMESTAMP DEFAULT NOW())",
                    targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translate sql server -> Impala stats reserved word",{
    sql <- translate("SELECT * FROM strata_stats AS stats",
                        targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "SELECT * FROM strata_stats AS _stats")
})

test_that("translate sql server -> Impala DATEFROMPARTS()", {
    sql <- translate("SELECT DATEFROMPARTS('1977', '10', '12')", 
                      targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "SELECT to_timestamp(CONCAT(CAST('1977' AS VARCHAR),'-',CAST('10' AS VARCHAR),'-',CAST('12' AS VARCHAR)), 'yyyy-M-d')")
  })

test_that("translate sql server -> Impala EOMONTH()", {
    sql <- translate("SELECT eomonth(payer_plan_period_start_date) AS obs_month_end",
                      targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "SELECT days_sub(add_months(trunc(CAST(payer_plan_period_start_date AS TIMESTAMP), 'MM'),1),1) AS obs_month_end")
  })

test_that("translate sql server -> Impala ISNUMERIC", {
    sql <- translate("SELECT ISNUMERIC(a) FROM b", targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "SELECT case when regexp_like(a,'^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') then 1 else 0 end FROM b")
    sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "SELECT some FROM table WHERE case when regexp_like(a,'^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') then 1 else 0 end = 1")
    sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "impala")
    expect_equal_ignore_spaces(sql, "SELECT some FROM table WHERE case when regexp_like(a,'^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') then 1 else 0 end = 0")
  })

test_that("translate sql server -> Impala data types", {
  sql <- translate("CREATE TABLE a (c1 DOUBLE PRECISION)",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 DOUBLE)")
})

test_that("translate sql server -> Impala escape chars", {
  sql <- translate("INSERT INTO t VALUES('some \"string\" ''with escape'' chars')", targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "INSERT INTO t VALUES(CONCAT('some \\042string\\042 ','\\047','with escape','\\047','chars'))")
})

# Netezza tests

test_that("translate sql server -> Netezza WITH cte AS () INSERT INTO tbl SELECT * FROM cte", {
  sql <- translate("WITH data AS (SELECT 'test' AS user, 'secret' AS password) INSERT INTO users SELECT * FROM data;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "INSERT INTO users WITH data AS (SELECT 'test' AS user, 'secret' AS password) SELECT * FROM data;")
})

test_that("translate sql server -> Netezza CAST(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101' , 'yyyymmdd');")
})

test_that("translate sql server -> Netezza DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translate sql server -> Netezza DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
})

test_that("translate sql server -> Netezza WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Netezza WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1  AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translate sql server -> Netezza WITH CTE SELECT INTO with RANDOM distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nWITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
                             "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE d \nAS\nWITH cte1  AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1\nDISTRIBUTE ON RANDOM;")
})

test_that("translate sql server -> Netezza WITH CTE SELECT INTO with KEY distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(c)\nWITH cte1 AS (SELECT a,c FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
                             "--HINT DISTRIBUTE_ON_KEY(c)\nCREATE TABLE d \nAS\nWITH cte1  AS (SELECT a,c FROM b)  SELECT\nc \nFROM\ncte1\nDISTRIBUTE ON (c);")
})

test_that("translate sql server -> Netezza WITH SELECT INTO with RANDOM distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nSELECT a INTO b FROM someTable;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
                             "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE b \nAS\nSELECT\na \nFROM\nsomeTable\nDISTRIBUTE ON RANDOM;")
})

test_that("translate sql server -> Netezza WITH SELECT INTO with KEY distribution", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(a)\nSELECT a INTO b FROM someTable;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
                             "--HINT DISTRIBUTE_ON_KEY(a)\nCREATE TABLE b \nAS\nSELECT\na \nFROM\nsomeTable\nDISTRIBUTE ON (a);")
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
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql,
                             "DROP TABLE cohort IF EXISTS;")
})

test_that("translate sql server -> Netezza LEFT functions", {
  sql <- translate("SELECT LEFT(x,4);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x, 1, 4);")
})

test_that("translate sql server -> Netezza RIGHT functions", {
  sql <- translate("SELECT RIGHT(x,4);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x, LENGTH(x) - 4 + 1, 4);")
})

test_that("translate sql server -> Netezza DELETE FROM WHERE", {
  sql <- translate("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "delete from ACHILLES_results where analysis_id IN (1, 2, 3);")
})

test_that("translate sql server -> Netezza CAST AS VARCHAR", {
  sql <- translate("CAST(person_id AS VARCHAR);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "CAST(person_id AS VARCHAR(1000));")
})

test_that("translate sql server -> PostgreSql TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> PostgreSql TOP subquery", {
  sql <- translate("SELECT name FROM (SELECT TOP 1 name FROM my_table WHERE a = b);",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT name FROM (SELECT name FROM my_table WHERE a = b LIMIT 1);")
})

test_that("translate sql server -> Oracle TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b AND ROWNUM <= 10; ")
})

test_that("translate sql server -> Oracle TOP subquery", {
  sql <- translate("SELECT name FROM (SELECT TOP 1 name FROM my_table WHERE a = b);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT name FROM (SELECT name FROM my_table WHERE a = b AND ROWNUM <= 1);")
})

test_that("translate sql server -> impala TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> netezza TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> netezza TOP subquery", {
  sql <- translate("SELECT * FROM (SELECT TOP 10 * FROM my_table WHERE a = b);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT * FROM (SELECT * FROM my_table WHERE a = b LIMIT 10);")
})

test_that("translate sql server -> netezza ISNUMERIC", {
    sql <- translate("SELECT ISNUMERIC(a) FROM b", targetDialect = "netezza")
    expect_equal_ignore_spaces(sql, "SELECT CASE WHEN translate(a,'0123456789','') in ('','.','-','-.') THEN 1 ELSE 0 END FROM b")
    sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "netezza")
    expect_equal_ignore_spaces(sql, "SELECT some FROM table WHERE CASE WHEN translate(a,'0123456789','') in ('','.','-','-.') THEN 1 ELSE 0 END = 1")
    sql <- translate("SELECT some FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "netezza")
    expect_equal_ignore_spaces(sql, "SELECT some FROM table WHERE CASE WHEN translate(a,'0123456789','') in ('','.','-','-.') THEN 1 ELSE 0 END = 0")
  })

test_that("translate sql server -> postgres date to varchar", {
  sql <- translate("SELECT CONVERT(VARCHAR,start_date,112) FROM table;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT TO_CHAR(start_date, 'YYYYMMDD') FROM table;")
})


test_that("translate sql server -> pdw hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translate sql server -> pdw hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE(row_id INT);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE (row_id INT)\nWITH (DISTRIBUTION = HASH(row_id));")
})

test_that("translate sql server -> pdw hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_RANDOM\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = ROUND_ROBIN) AS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translate sql server -> pdw hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE(row_id INT);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_RANDOM\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE (row_id INT)\nWITH (DISTRIBUTION = ROUND_ROBIN);")
})


test_that("translate sql server -> redshift hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE  #my_table\nDISTKEY(row_id)\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translate sql server -> redshift hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE my_table (row_id INT);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE my_table (row_id INT)\nDISTKEY(row_id);")
})

test_that("translate sql server -> redshift hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE  #my_table\nDISTSTYLE EVEN\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translate sql server -> redshift hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE my_table (row_id INT);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE my_table (row_id INT)\nDISTSTYLE EVEN;")
})


test_that("translate: warning on temp table name that is too long", {
  expect_warning(translate("SELECT * FROM #abcdefghijklmnopqrstuvwxyz", "pdw"))
})

test_that("translate: warning on table name that is too long", {
  expect_warning(translate("DROP TABLE abcdefghijklmnopqrstuvwxyz123456789", "pdw"))
})


test_that("translate sql server -> oracle concat", {
  sql <- translate("SELECT CONCAT(a,\" , \",c,d,e) FROM x;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT CONCAT(a, CONCAT(\" , \", CONCAT( c, CONCAT( d, e)))) FROM x;")
})

test_that("translate sql server -> oracle natural log", {
  sql <- translate("SELECT LOG(number) FROM table",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT LOG(2.718281828459,number) FROM table")
})

test_that("translate sql server -> oracle log base 10", {
  sql <- translate("SELECT LOG10(number) FROM table;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT LOG(10,number) FROM table;")
})

test_that("translate sql server -> oracle log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT LOG( base,number) FROM table")
})

test_that("translate sql server -> redshift natural log", {
  sql <- translate("SELECT LOG(number) FROM table",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((number) AS REAL)) FROM table")
})

test_that("translate sql server -> redshift log base 10", {
  sql <- translate("SELECT LOG10(number) FROM table;",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT LOG(CAST((number) AS REAL)) FROM table;")
})

test_that("translate sql server -> redshift log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT (LN(CAST((number) AS REAL))/LN(CAST(( base) AS REAL))) FROM table")
})

test_that("translate sql server -> postgresql natural log", {
  sql <- translate("SELECT LOG(number) FROM table",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((number) AS REAL)) FROM table")
})

test_that("translate sql server -> postgresql log base 10", {
  sql <- translate("SELECT LOG10(number) FROM table;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT LOG(10,CAST((number) AS NUMERIC)) FROM table;")
})

test_that("translate sql server -> postgresql log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT LOG(CAST(( base) AS NUMERIC),CAST((number) AS NUMERIC)) FROM table")
})

test_that("translate sql server -> oracle union 1", {
  sql <- translate("SELECT * FROM table1 WHERE a = 1 UNION SELECT * FROM table2 WHERE a = 1;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT * FROM table1 WHERE a = 1 UNION SELECT * FROM table2 WHERE a = 1;")
})

test_that("translate sql server -> oracle union 2", {
  sql <- translate("SELECT * FROM table1 UNION SELECT * FROM table2 WHERE a = 1;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT * FROM table1 UNION SELECT * FROM table2 WHERE a = 1;")
})

test_that("translate sql server -> oracle from dual", {
  sql <- translate("SELECT 1 AS id;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT 1 AS id FROM DUAL;")
})


test_that("translate sql server -> oracle from dual", {
  sql <- translate("SELECT (SELECT id FROM a WHERE b=2) AS id;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT (SELECT id FROM a WHERE b=2) AS id FROM DUAL;")
})

test_that("translate sql server -> oracle ISNUMERIC", {
  sql <- translate("SELECT CASE WHEN ISNUMERIC(a) = 1 THEN a ELSE b FROM c;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN 1 ELSE 0 END = 1 THEN a ELSE b  FROM c ;")
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a FROM table WHERE CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN 1 ELSE 0 END = 1")
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a FROM table WHERE CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN 1 ELSE 0 END = 0")
})

test_that("translate sql server -> postgres ISNUMERIC", {
  sql <- translate("SELECT CASE WHEN ISNUMERIC(a) = 1 THEN a ELSE b FROM c;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 1 THEN a ELSE b FROM c;")
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT a FROM table WHERE CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 1")
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "SELECT a FROM table WHERE CASE WHEN (CAST(a AS VARCHAR) ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN 1 ELSE 0 END = 0")
})

test_that("translate sql server -> bigquery lowercase all but strings and variables", {
  sql <- translate("SELECT X.Y, 'Mixed Case String' FROM \"MixedCaseTableName.T\" where x.z=@camelCaseVar GROUP BY X.Y",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select x.y, 'Mixed Case String' from \"MixedCaseTableName.T\" where x.z=@camelCaseVar group by x.y")
})

test_that("translate sql server -> bigquery common table expression column list", {
  sql <- translate("with cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "with cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;")
})

test_that("translate sql server -> bigquery multiple common table expression column list", {
  sql <- translate("with cte1 as (select 2), cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "with cte1 as (select 2), cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;")
})

test_that("translate sql server -> bigquery distinct keyword", {
    sql <- translate("with cte2 (column1, column2) as (select distinct c1.column1, c1.column2 from cte c1) select column1, column2 into cte2 from cte2",
    targetDialect = "bigquery")
    expect_equal_ignore_spaces(sql, "with cte2  as (select distinct c1.column1 as column1,c1.column2  as column2 from cte c1) select column1, column2 into cte2 from cte2")
})

test_that("translate sql server -> bigquery group by function", {
  sql <- translate("select f(a), count(*) from t group by f(a);",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select f(a), count(*) from t group by 1;")
})

test_that("translate sql server -> bigquery group by addition", {
  sql <- translate("select 100, sum(x), cast(a+b as string) from t group by a+b;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select 100, sum(x), cast(a+b as string) from t group by 3;")
})

test_that("translate sql server -> bigquery column ref groupby", {
  sql <- translate("select 100, sum(x), cast(a+b as string) from t group by t.a, t.b;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select 100, sum(x), cast(a+b as string) from t group by t.a, t.b;")
})

test_that("translate sql server -> bigquery group by without match", {
  sql <- translate("select 100, sum(x), concat('count = ', c) from t group by a+b;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select 100, sum(x), concat('count = ', c) from t group by a + b;")
})

test_that("translate sql server -> bigquery group by without final semicolon", {
  sql <- translate("select f(a) from t group by f(a);",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select f(a) from t group by 1;")
})

test_that("translate sql server -> bigquery order by", {
  sql <- translate("select f(a) from t group by f(a) order by f(a);",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select f(a) from t group by 1 order by 1;")
})

test_that("translate sql server -> bigquery nested group by", {
  sql <- translate("select * from (select 100, cast(a+b as string), max(x) from t group by a+b) dt;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select * from (select 100, cast(a+b as string), max(x) from t group by 2) dt;")
})

test_that("translate sql server -> bigquery complex group by", {
  sql <- translate("select 100, 200, cast(floor(date_diff(a, b, day)/30) string string), 300 from t group by floor(date_diff(a, b, day)/30);",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select 100, 200, cast(floor(date_diff(a, b, day)/30) string string), 300 from t group by 3;")
})

test_that("translate sql server -> bigquery group by having", {
  sql <- translate(paste(
                        "select cast(stratum_1 as integer) as concept_id, sum(count_value) as count_value ",
                        "from heracles_results ",
                        "where analysis_id in (123) ",
                        "group by cast(stratum_1 as integer) ",
                        "having sum(count_value) > 1;"
                      ),
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, paste(
                        "select cast(stratum_1 as INT64) as concept_id, sum(count_value) as count_value ",
                        "from heracles_results ",
                        "where analysis_id in (123) ",
                        "group by 1 ",
                        "having sum(count_value) > 1;"
                      ))
})

test_that("translate sql server -> bigquery column references", {
  sql <- translate("select concat(t.a, t.b) from t group by t.a, t.b;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select concat(t.a, t.b) from t group by t.a, t.b;")
})

test_that("translate sql server -> bigquery mixed column references", {
  sql <- translate("select concat(t.a, t.b), x+y+z from t group by t.a, t.b, x+y+z;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select concat(t.a, t.b), x+y+z from t group by t.a, t.b, 2;")
})

test_that("translate sql server -> bigquery DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select DATE_DIFF(cast(drug_era_end_date as date), cast(drug_era_start_date as date), DAY) from drug_era;")
})

test_that("translate sql server -> bigquery DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select DATE_ADD(cast(drug_era_end_date as date), interval 30 DAY) from drug_era;")
})

test_that("translate sql server -> bigquery GETDATE", {
  sql <- translate("GETDATE()",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "CURRENT_DATE()")
})

test_that("translate sql server -> bigquery STDEV", {
  sql <- translate("stdev(x)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "STDDEV(x)")
})

test_that("translate sql server -> bigquery LEN", {
  sql <- translate("len('abc')",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "LENGTH('abc')")
})

test_that("translate sql server -> bigquery COUNT_BIG", {
  sql <- translate("COUNT_BIG(x)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "COUNT(x)")
})

test_that("translate sql server -> bigquery CAST varchar", {
  sql <- translate("select cast(x as varchar)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select cast(x as STRING)")
})

test_that("translate sql server -> bigquery CAST :float", {
  sql <- translate("select cast(x as:float)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select CAST(x as float64)")
})

test_that("translate sql server -> bigquery DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS cohort;")
})

test_that("translate sql server -> bigquery CAST string", {
  sql <- translate("CAST(x AS VARCHAR(255))",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(x as STRING)")
})

test_that("translate sql server -> bigquery LEFT, RIGHT", {
  sql <- translate("select LEFT(a, 20), RIGHT(b, 30) FROM t;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select SUBSTR(a, 0, 20), SUBSTR(b, -30) from t;")
})

test_that("translate sql server -> bigquery cast float", {
  sql <- translate("cast(a as float)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(a as float64)")
})

test_that("translate sql server -> bigquery cast bigint", {
  sql <- translate("cast(a as bigint)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(a as int64)")
})

test_that("translate sql server -> bigquery cast int", {
  sql <- translate("cast(a as int)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(a as int64)")
})

test_that("translate sql server -> bigquery cast date", {
  sql <- translate("date(d)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(d as date)")
})

test_that("translate sql server -> bigquery cast string as date", {
  sql <- translate("cast(concat(a,b) as date)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "parse_date('%Y%m%d', concat(a,b))")
})

test_that("translate sql server -> bigquery extract year", {
  sql <- translate("year(d)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "EXTRACT(YEAR from d)")
})

test_that("translate sql server -> bigquery extract month", {
  sql <- translate("month(d)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "EXTRACT(MONTH from d)")
})

test_that("translate sql server -> bigquery extract day", {
  sql <- translate("day(d)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "EXTRACT(DAY from d)")
})

test_that("translate sql server -> bigquery union distinct", {
  sql <- translate("select 1 as x union select 2;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select 1 as x union distinct select 2;")
})

test_that("translate sql server -> bigquery intersect distinct", {
  sql <- translate("SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2;")
})

test_that("translate sql server -> bigquery bracketed intersect distinct", {
  sql <- translate("(SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "(SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2)")
})

test_that("translate sql server -> bigquery isnull", {
  sql <- translate("SELECT isnull(x,y) from t;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select IFNULL(x,y) from t;")
})

test_that("translate sql server -> bigquery unquote aliases", {
  sql <- translate("SELECT a as \"b\" from t;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select a as b from t;")
})

test_that("translate sql server -> bigquery cast int in coalesce", {
  sql <- translate("select coalesce(x, 0), coalesce(12, y) from t",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select coalesce(cast(x as int64), 0), coalesce(12, cast(y as int64)) from  t")
})

test_that("translate sql server -> bigquery cast decimal", {
  sql <- translate("select cast(x as decimal(18,4)) from t",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select cast(x as float64) from t")
})

test_that("translate sql server -> bigquery ISNUMERIC", {
    sql <- translate("select ISNUMERIC(a) from b", targetDialect = "bigquery")
    expect_equal_ignore_spaces(sql, "select CASE WHEN SAFE_CAST(a AS FLOAT64) IS NULL THEN 0 ELSE 1 END from b")
    sql <- translate("select a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "bigquery")
    expect_equal_ignore_spaces(sql, "select a from table where CASE WHEN SAFE_CAST(a AS FLOAT64) IS NULL THEN 0 ELSE 1 END = 1")
    sql <- translate("select a FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "bigquery")
    expect_equal_ignore_spaces(sql, "select a from table where CASE WHEN SAFE_CAST(a AS FLOAT64) IS NULL THEN 0 ELSE 1 END = 0")
  })


test_that("translate sql server -> bigquery index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "-- bigquery does not support indexes")
})

test_that("translate sql server -> bigquery TRUNCATE TABLE", {
  sql <- translate("TRUNCATE TABLE cohort;",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "DELETE FROM cohort WHERE True;")
})

test_that("translate sql server -> bigquery DATEFROMPARTS", {
  sql <- translate("select DATEFROMPARTS(2019,1,30)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select DATE(2019,1,30)")
})

test_that("translate sql server -> bigquery EOMONTH()", {
  sql <- translate("select eomonth(payer_plan_period_start_date)",
                      targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select DATE_SUB(DATE_TRUNC(DATE_ADD(payer_plan_period_start_date, INTERVAL 1 MONTH), MONTH), INTERVAL 1 DAY)")
})

test_that("translate sql server -> bigquery escape chars", {
  sql <- translate("INSERT INTO t VALUES('some \"string\" ''with escape'' chars')", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "insert into t values(CONCAT('some \\042string\\042 ','\\047','with escape','\\047','chars'))")
})

test_that("translate sql server -> RedShift DATEADD dd", {
  sql <- translate("SELECT DATEADD(dd, 30, drug_era_end_date) FROM drug_era;",targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(day, CAST(30 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD mm", {
  sql <- translate("SELECT DATEADD(mm, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(month, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD m", {
  sql <- translate("SELECT DATEADD(m, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(month, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD yyyy", {
  sql <- translate("SELECT DATEADD(yyyy, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(year, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD yy", {
  sql <- translate("SELECT DATEADD(yy, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(year, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD qq", {
  sql <- translate("SELECT DATEADD(qq, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(quarter, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD q", {
  sql <- translate("SELECT DATEADD(q, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(quarter, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD wk", {
  sql <- translate("SELECT DATEADD(wk, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(week, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD ww", {
  sql <- translate("SELECT DATEADD(ww, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(week, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD hh", {
  sql <- translate("SELECT DATEADD(hh, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(hour, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD mi", {
  sql <- translate("SELECT DATEADD(mi, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(minute, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD ss", {
  sql <- translate("SELECT DATEADD(ss, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(second, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEADD mcs", {
  sql <- translate("SELECT DATEADD(mcs, 3, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(microsecond, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF dd", {
  sql <- translate("SELECT DATEDIFF(dd, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF m", {
  sql <- translate("SELECT DATEDIFF(m, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF mm", {
  sql <- translate("SELECT DATEDIFF(mm, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF yyyy", {
  sql <- translate("SELECT DATEDIFF(yyyy, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF yy", {
  sql <- translate("SELECT DATEDIFF(yy, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF qq", {
  sql <- translate("SELECT DATEDIFF(qq, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF q", {
  sql <- translate("SELECT DATEDIFF(q, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF wk", {
  sql <- translate("SELECT DATEDIFF(wk, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF ww", {
  sql <- translate("SELECT DATEDIFF(ww, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF hh", {
  sql <- translate("SELECT DATEDIFF(hh, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF mi", {
  sql <- translate("SELECT DATEDIFF(mi, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF n", {
  sql <- translate("SELECT DATEDIFF(n, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF ss", {
  sql <- translate("SELECT DATEDIFF(ss, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF mcs", {
  sql <- translate("SELECT DATEDIFF(mcs, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(microsecond, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG dd", {
  sql <- translate("SELECT DATEDIFF_BIG(dd, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG m", {
  sql <- translate("SELECT DATEDIFF_BIG(m, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG mm", {
  sql <- translate("SELECT DATEDIFF_BIG(mm, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG yyyy", {
  sql <- translate("SELECT DATEDIFF_BIG(yyyy, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG yy", {
  sql <- translate("SELECT DATEDIFF_BIG(yy, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG qq", {
  sql <- translate("SELECT DATEDIFF_BIG(qq, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG q", {
  sql <- translate("SELECT DATEDIFF_BIG(q, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG wk", {
  sql <- translate("SELECT DATEDIFF_BIG(wk, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG ww", {
  sql <- translate("SELECT DATEDIFF_BIG(ww, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG hh", {
  sql <- translate("SELECT DATEDIFF_BIG(hh, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG mi", {
  sql <- translate("SELECT DATEDIFF_BIG(mi, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG n", {
  sql <- translate("SELECT DATEDIFF_BIG(n, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG ss", {
  sql <- translate("SELECT DATEDIFF_BIG(ss, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEDIFF_BIG mcs", {
  sql <- translate("SELECT DATEDIFF_BIG(mcs, drug_era_start_date, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(microsecond, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART dd", {
  sql <- translate("SELECT DATEPART(dd, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(day, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART m", {
  sql <- translate("SELECT DATEPART(m, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(month, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART mm", {
  sql <- translate("SELECT DATEPART(mm, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(month, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART yyyy", {
  sql <- translate("SELECT DATEPART(yyyy, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(year, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART yy", {
  sql <- translate("SELECT DATEPART(yy, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(year, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART qq", {
  sql <- translate("SELECT DATEPART(qq, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(quarter, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART q", {
  sql <- translate("SELECT DATEPART(q, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(quarter, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART wk", {
  sql <- translate("SELECT DATEPART(wk, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(week, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART ww", {
  sql <- translate("SELECT DATEPART(ww, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(week, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART hh", {
  sql <- translate("SELECT DATEPART(hh, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(hour, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART mi", {
  sql <- translate("SELECT DATEPART(mi, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART n", {
  sql <- translate("SELECT DATEPART(n, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART ss", {
  sql <- translate("SELECT DATEPART(ss, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(second, drug_era_end_date) FROM drug_era;")
})

test_that("translate sql server -> RedShift DATEPART mcs", {
  sql <- translate("SELECT DATEPART(mcs, drug_era_end_date) FROM drug_era;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(microsecond, drug_era_end_date) FROM drug_era;")
})


test_that("translate sql server -> RedShift DATETIMEFROMPARTS", {
  sql <- translate("SELECT DATETIMEFROMPARTS(year,month,day,hour,minute,second,millisecond) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(second,'00FM')||'.'||TO_CHAR(millisecond,'000FM') as TIMESTAMP) FROM table")
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
  sql <- translate("CREATE TABLE table ( col DATETIMEOFFSET(@p) not null)", targetDialect = "redshift")
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
  sql <- translate("CREATE TABLE table ( col UNIQUEIDENTIFIER not null)", targetDialect = "redshift")
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
  sql <- translate("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds, 0, 0) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM') as TIMESTAMP) FROM table")
})

test_that("translate sql server -> RedShift DATETIME2FROMPARTS with fractions", {
  sql <- translate("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds,fractions,precision) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0', precision) || 'FM') as TIMESTAMP) FROM table")
})

test_that("translate sql server -> RedShift DATETIMEOFFSETFROMPARTS", {
  sql <- translate("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds, 0,h_offset,m_offset, 0) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table")
})

test_that("translate sql server -> RedShift DATETIMEOFFSETFROMPARTS with fractions", {
  sql <- translate("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds,fractions,h_offset,m_offset,precision) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0',precision) || 'FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table")
})

test_that("translate sql server -> RedShift GETUTCDATE", {
  sql <- translate("SELECT GETUTCDATE();", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translate sql server -> RedShift SMALLDATETIMEFROMPARTS", {
  sql <- translate("SELECT SMALLDATETIMEFROMPARTS(year,month,day,hour,minute) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM') as TIMESTAMP) FROM table")
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
  expect_equal_ignore_spaces(sql, "SELECT case when trunc = 0 then ROUND(CAST(expression AS FLOAT),length) else TRUNC(CAST(expression AS FLOAT),length) end FROM table")
})

test_that("translate sql server -> RedShift CHARINDEX from position", {
  sql <- translate("SELECT CHARINDEX('test',column, 3) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT case when CHARINDEX('test', SUBSTRING(column, 3)) > 0 then (CHARINDEX('test', SUBSTRING(column, 3)) + 3 - 1) else 0 end FROM table")
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
  sql <- translate("SELECT STUFF(expression, start, length, replace) FROM table", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTRING(expression, 0, start)|| replace||SUBSTRING(expression, start + length) FROM table")
})

test_that("translate sql server -> RedShift CONCAT", {
  sql <- translate(
    "SELECT CONCAT(p1,p2,p3,p4,p5,p6,p7) FROM table", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CONCAT(p1,CONCAT(p2,CONCAT(p3,CONCAT(p4,CONCAT(p5,CONCAT(p6,p7)))))) FROM table")
})

test_that("translate sql server -> RedShift CONCAT", {
  sql <- translate(
    "SELECT CONCAT('Condition occurrence record observed during long_term_days on or prior to cohort index:  ', CAST((p1.covariate_id-101)/1000 AS VARCHAR), '-', CASE WHEN c1.concept_name IS NOT NULL THEN c1.concept_name ELSE 'Unknown invalid concept' END) FROM table", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "SELECT CONCAT('Condition occurrence record observed during long_term_days on or prior to cohort index:  ',CONCAT(CAST((p1.covariate_id-101)/1000 AS VARCHAR),CONCAT('-',CASE WHEN c1.concept_name IS NOT NULL THEN c1.concept_name ELSE 'Unknown invalid concept' END))) FROM table")
})




test_that("translate sql server -> RedShift CTAS TEMP WITH CTE person_id", {
  sql <- translate(
    "WITH a AS b SELECT person_id, col1, col2 INTO #table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS TEMP WITH CTE person_id at the end", {
  sql <- translate(
    "WITH a AS b SELECT col1, col2, person_id INTO #table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  col1, col2, person_id\nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id", {
  sql <- translate(
    "WITH a AS b SELECT person_id, col1, col2 INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias", {
  sql <- translate(
    "WITH a AS b SELECT person_id as dist, col1, col2 INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id as dist, col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias at the end", {
  sql <- translate(
    "WITH a AS b SELECT col1, col2, person_id as dist INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, col2, person_id as dist \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias (no 'as')", {
  sql <- translate(
    "WITH a AS b SELECT col1, person_id dist, col2 INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, person_id dist, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS WITH CTE person_id with alias (no 'as') at the end", {
  sql <- translate(
    "WITH a AS b SELECT col1, col2, person_id dist INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, col2, person_id dist \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS TEMP person_id", {
  sql <- translate(
    "SELECT person_id, col1, col2 INTO #table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS person_id", {
  sql <- translate(
    "SELECT person_id, col1, col2 INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(person_id)\nAS\nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS person_id with alias", {
  sql <- translate(
    "SELECT person_id as dist, col1, col2 INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n  person_id as dist, col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS person_id with alias at the end", {
  sql <- translate(
    "SELECT col1, col2, person_id as dist INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n col1, col2, person_id as dist \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS person_id with alias (no 'as')", {
  sql <- translate(
    "SELECT person_id dist, col1, col2 INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n  person_id dist, col1, col2 \nFROM\n person;")
})

test_that("translate sql server -> RedShift CTAS person_id with alias (no 'as') at the end", {
  sql <- translate(
    "SELECT col1, col2, person_id dist INTO table FROM person;", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n col1, col2, person_id dist \nFROM\n person;")
})

test_that("translate sql server -> RedShift CREATE TABLE person_id", {
  sql <- translate(
    "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  [dbo].[drug_era]  ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nDISTKEY(person_id);")
})

test_that("translate sql server -> PDW CREATE TABLE person_id", {
  sql <- translate(
    "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);", 
    targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, 
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   [dbo].[drug_era]  ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translate sql server -> RedShift ISDATE", {
  sql <- translate(
    "SELECT * FROM table WHERE ISDATE(col) = 1", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "SELECT * FROM table WHERE REGEXP_INSTR(col, '^(\\\\d{4}[/\\-]?[01]\\\\d[/\\-]?[0123]\\\\d)([ T]([0-1][0-9]|[2][0-3]):([0-5][0-9])(:[0-5][0-9](.\\\\d+)?)?)?$') = 1")
})

test_that("translate sql server -> RedShift ISNUMERIC", {
  sql <- translate(
    "SELECT * FROM table WHERE ISNUMERIC(col) = 1", 
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "SELECT * FROM table WHERE REGEXP_INSTR(col, '^[\\-\\+]?(\\\\d*\\\\.)?\\\\d+([Ee][\\-\\+]?\\\\d+)?$') = 1")
})

test_that("translate sql server -> RedShift PATINDEX", {
  sql <- translate(
    "SELECT PATINDEX(pattern,expression) FROM table;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "SELECT REGEXP_INSTR(expression, case when LEFT(pattern,1)<>'%' and RIGHT(pattern,1)='%' then '^' else '' end||TRIM('%' FROM REPLACE(pattern,'_','.'))||case when LEFT(pattern,1)='%' and RIGHT(pattern,1)<>'%' then '$' else '' end) FROM table;")
})

test_that("translate sql server -> RedShift SELECT INTO temp table with CTE and default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste(
    "WITH cte(a1) AS (SELECT a1 FROM table_a)",
    "SELECT *",
    "INTO #table",
    "FROM cte;",
    sep = " "),
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  #table  DISTSTYLE ALL",
    "AS",
    "WITH",
    " cte(a1) ",
    "AS",
    " (SELECT a1 FROM table_a) ",
    "SELECT",
    " * ",
    "FROM",
    " cte;",
    sep = "\n"))
})

test_that("translate sql server -> RedShift SELECT INTO permanent table with CTE and default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste(
    "WITH cte(a1) AS (SELECT a1 FROM table_a)",
    "SELECT *",
    "INTO table",
    "FROM cte;",
    sep = " "),
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  table  DISTSTYLE ALL",
    "AS",
    "WITH",
    " cte(a1) ",
    "AS",
    " (SELECT a1 FROM table_a) ",
    "SELECT",
    " * ",
    "FROM",
    " cte;",
    sep = "\n"))
})

test_that("translate sql server -> RedShift SELECT INTO temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste(
    "SELECT *",
    "INTO #table",
    "FROM another_table;",
    sep = " "),
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  #table  DISTSTYLE ALL",
    "AS",
    "SELECT",
    " * ",
    "FROM",
    " another_table;",
    sep = "\n"))
})

test_that("translate sql server -> RedShift SELECT INTO permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(paste(
    "SELECT *",
    "INTO table",
    "FROM another_table;",
    sep = " "),
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  table  DISTSTYLE ALL",
    "AS",
    "SELECT",
    " * ",
    "FROM",
    " another_table;",
    sep = "\n"))
})

test_that("translate sql server -> RedShift SELECT value INTO temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(
    "SELECT a INTO #table;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table DISTSTYLE ALL\nAS\nSELECT\n a ;")
})

test_that("translate sql server -> RedShift SELECT value INTO permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(
    "SELECT a INTO table;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table DISTSTYLE ALL\nAS\nSELECT\n a ;")
})

test_that("translate sql server -> RedShift create temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(
    "CREATE TABLE #table (id int not null, col varchar(max));",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table  (id int not null, col varchar(max))\nDISTSTYLE ALL;")
})

test_that("translate sql server -> RedShift create permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translate(
    "CREATE TABLE table (id int not null, col varchar(max));",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table  (id int not null, col varchar(max))\nDISTSTYLE ALL;")
})

test_that("translate sql server -> RedShift CREATE TABLE IF NOT EXISTS with hashing", {
  sql <- translate(paste(    
    "IF OBJECT_ID('dbo.heracles_results', 'U') IS NULL",
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
    sep = "\n"),
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  IF NOT EXISTS  dbo.heracles_results",
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
    sep = "\n"))
})

test_that("translate sql server -> RedShift DISTINCT + TOP", {
  sql <- translate(
    "SELECT DISTINCT TOP 100 * FROM table WHERE a = b;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "SELECT TOP 100 DISTINCT * FROM table WHERE a = b;")
})

test_that("Postgres String literal within CTE should be explicitly casted to character type", {
  sql <- translate(
    "WITH expression AS(SELECT 'my literal', col1, CAST('other literal' as VARCHAR(MAX)), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;",
    targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, 
                             "WITH  expression  AS (SELECT CAST('my literal' as TEXT), col1, CAST('other literal' as TEXT), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;")
})

test_that("RedShift XOR operator", {
  sql <- translate("select a ^ b from c where a = 1;", targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select a # b from c where a = 1;")
})

test_that("translate sql server -> redshift hint DISTKEY + SORTKEY on CTAS + CTE", {
  sql <- translate(
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(COMPOUND:start_date)\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT * INTO #my_table FROM cte;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(COMPOUND:start_date)\nCREATE TABLE #my_table\nDISTKEY(row_id)\nCOMPOUND SORTKEY(start_date)\nAS\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT\n * \nFROM\n cte;")
})

test_that("translate sql server -> redshift hint SORTKEY on CTAS + CTE", {
  sql <- translate(
    "--HINT SORT_ON_KEY(COMPOUND:start_date)\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT * INTO #my_table FROM cte;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "--HINT SORT_ON_KEY(COMPOUND:start_date)\nCREATE TABLE #my_table\nCOMPOUND SORTKEY(start_date)\nAS\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT\n * \nFROM\n cte;")
})

test_that("translate sql server -> redshift hint DISTKEY + SORTKEY on CTAS", {
  sql <- translate(
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(:start_date, end_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(:start_date, end_date)\nCREATE TABLE #my_table\nDISTKEY(row_id)\nSORTKEY(start_date, end_date)\nAS\nSELECT\n*\nFROM\n other_table;")
})

test_that("translate sql server -> redshift hint SORTKEY on CTAS", {
  sql <- translate(
    "--HINT SORT_ON_KEY(:start_date, end_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, 
                             "--HINT SORT_ON_KEY(:start_date, end_date)\nCREATE TABLE #my_table\nSORTKEY(start_date, end_date)\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translate sql server -> redshift hint DISTKEY + SORTKEY on CREATE TABLE", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date)\nDISTKEY(row_id)\nINTERLEAVED SORTKEY(start_date);")
})

test_that("translate sql server -> redshift hint SORTKEY on CREATE TABLE", {
  sql <- translate("--HINT SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "--HINT SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date)\n\nINTERLEAVED SORTKEY(start_date);")
})

test_that("translate sql server -> pdw hint DISTKEY + SORTKEY on CREATE TABLE", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nCREATE TABLE my_table (row_id INT, start_date DATE);",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\n\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE my_table (row_id INT, start_date DATE)\nWITH (DISTRIBUTION = HASH(row_id));")
})

test_that("translate sql server -> pdw hint DISTKEY + SORTKEY on CTAS", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\n\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translate sql server -> redshift CONVERT to DATE", {
  sql <- translate("select CONVERT(DATE, start_date) from my_table;",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select CAST(start_date as DATE) from my_table;")
})

test_that("translate sql server -> redshift CONVERT to TIMESTAMPTZ", {
  sql <- translate("select CONVERT(TIMESTAMPTZ, start_date) from my_table;",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select CONVERT(TIMESTAMP WITH TIME ZONE, start_date) from my_table;")
})

test_that("translate sql server -> oracle add group by when case count", {
  sql <- translate("SELECT CASE COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1) END AS stat FROM my_table;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT  CASE  COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1)  END AS stat  FROM my_table  GROUP BY 1;")
})

test_that("translate sql server -> oracle don't add group by when case count but already group by", {
  sql <- translate("SELECT CASE COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1) END AS stat FROM my_table GROUP BY y;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT  CASE  COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1)  END AS stat  FROM my_table GROUP BY y  ;")
})

test_that("translate sql server -> Redshift partition window function sorted descending", {
  sql <- translate("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt desc) as count_value", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  DESC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translate sql server -> Redshift partition window function sorted ascending", {
  sql <- translate("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt asc) as count_value", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  ASC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translate sql server -> Redshift partition window function no sort specified", {
  sql <- translate("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt) as count_value", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translate sql server -> Redshift partition window function with specified frame", {
  sql <- translate("select MAX(start_ordinal) OVER (PARTITION BY groupid ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select MAX(start_ordinal) OVER (PARTITION BY groupid ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal")
})

test_that("translate sql server -> Redshift partition window function ROW_NUMBER no sort specified", {
  sql <- translate("select ROW_NUMBER() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select ROW_NUMBER() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function CUME_DIST no sort specified", {
  sql <- translate("select CUME_DIST() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select CUME_DIST() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function DENSE_RANK no sort specified", {
  sql <- translate("select DENSE_RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select DENSE_RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function PERCENT_RANK no sort specified", {
  sql <- translate("select PERCENT_RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select PERCENT_RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function RANK no sort specified", {
  sql <- translate("select RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function LAG no sort specified", {
  sql <- translate("select LAG(mycol) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select LAG(mycol) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function LEAD no sort specified", {
  sql <- translate("select LEAD(mycol) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select LEAD(mycol) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift partition window function NTILE no sort specified", {
  sql <- translate("select NTILE(4) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select NTILE(4) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function sorted descending without partition by clause", {
  sql <- translate("select sum(count(person_id)) over (order by prc_cnt desc) as count_value", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (ORDER BY prc_cnt  DESC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translate sql server -> Redshift window function sorted ascending without partition by clause", {
  sql <- translate("select sum(count(person_id)) over (order by prc_cnt asc) as count_value", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (ORDER BY prc_cnt  ASC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translate sql server -> Redshift window function no sort specified without partition by clause", {
  sql <- translate("select sum(count(person_id)) over (order by prc_cnt) as count_value", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (ORDER BY prc_cnt ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translate sql server -> Redshift window function ROW_NUMBER no sort specified without PARTITION BY clause", {
  sql <- translate("select ROW_NUMBER() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select ROW_NUMBER() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function CUME_DIST no sort specified without PARTITION BY clause", {
  sql <- translate("select CUME_DIST() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select CUME_DIST() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function DENSE_RANK no sort specified without PARTITION BY clause", {
  sql <- translate("select DENSE_RANK() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select DENSE_RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function PERCENT_RANK no sort specified without PARTITION BY clause", {
  sql <- translate("select PERCENT_RANK() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select PERCENT_RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function RANK no sort specified without PARTITION BY clause", {
  sql <- translate("select RANK() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function LAG no sort specified without PARTITION BY clause", {
  sql <- translate("select LAG(mycol) over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select LAG(mycol) OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function LEAD no sort specified without PARTITION BY clause", {
  sql <- translate("select LEAD(mycol) over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select LEAD(mycol) OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Redshift window function NTILE no sort specified without PARTITION BY clause", {
  sql <- translate("select NTILE(4) over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "select NTILE(4) OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translate sql server -> Oracle union of two queries without FROM", {
  sql <- translate("SELECT 1,2 UNION SELECT 3,4;", 
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL;")
})

test_that("translate sql server -> Oracle union of three queries without FROM", {
  sql <- translate("SELECT 1,2 UNION SELECT 3,4 UNION SELECT 5,6;", 
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL UNION SELECT 5,6 FROM DUAL;")
})


test_that("translate sql server -> Oracle insert plus union of three queries without FROM", {
  sql <- translate("INSERT INTO my_table (a, b) SELECT 1,2 UNION SELECT 3,4 UNION SELECT 5,6;", 
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "INSERT INTO my_table (a, b) SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL UNION SELECT 5,6 FROM DUAL;")
})

test_that("translate sql server -> Oracle union where only last query needs FROM DUAL", {
  sql <- translate("SELECT a,b FROM my_table UNION SELECT 5,6;", 
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a,b FROM my_table UNION SELECT 5,6 FROM DUAL;")
})

test_that("translate sql server -> Oracle nested queries with EOLs", {
  sql <- translate("INSERT INTO test (a,b) SELECT a,b FROM (SELECT a,b FROM (SELECT a,b FROM my_table\n) nesti WHERE b = 2\n) nesto WHERE a = 1;", 
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "INSERT INTO test (a,b) SELECT a,b FROM (SELECT a,b FROM (SELECT a,b FROM my_table\n ) nesti WHERE b = 2\n ) nesto WHERE a = 1;")
})

test_that("translate sql server -> Oracle nested queries with union", {
  sql <- translate("SELECT a,b FROM (SELECT a,b FROM x UNION ALL SELECT a,b FROM x) o;", 
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a,b FROM (SELECT a,b FROM x UNION ALL SELECT a,b FROM x) o;")
})

test_that("translate sql server -> Netezza concat with more than two arguments", {
  sql <- translate("SELECT CONCAT(a,b,c,d,e) FROM x;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT a || b || c || d || e FROM x;")
})

test_that("translate sql server -> Netezza nested concat ", {
  sql <- translate("SELECT CONCAT(CONCAT(CONCAT(a,CONCAT(b,c)),d),e) FROM x;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "SELECT a || b || c || d || e FROM x;")
})

test_that("translate sql server -> Netezza clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "-- netezza does not support indexes")
})

test_that("translate sql server -> Netezza index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "-- netezza does not support indexes")
})

test_that("translate sql server -> Redshift clustered index not supported", {
  sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "-- redshift does not support indexes")
})

test_that("translate sql server -> Redshift index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "-- redshift does not support indexes")
})

test_that("translate sql server -> Oracle BIGINT in conditional create table", {
  sql <- translate("IF OBJECT_ID('test', 'U') IS NULL CREATE TABLE test (	x BIGINT);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "BEGIN\n  EXECUTE IMMEDIATE 'CREATE TABLE test  (x NUMBER(19))';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -955 THEN\n      RAISE;\n    END IF;\nEND;")
})

test_that("translate sql server -> Oracle NOT NULL and DEFAULT in conditional create table", {
  sql <- translate("IF OBJECT_ID('test_b', 'U') IS NULL CREATE TABLE test_b (x INT NOT NULL DEFAULT 0);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "BEGIN\n  EXECUTE IMMEDIATE 'CREATE TABLE test_b  (x INT DEFAULT 0 NOT NULL)';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -955 THEN\n      RAISE;\n    END IF;\nEND;")
})

test_that("translate sql server -> Oracle analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "-- ANALYZE should not be used to collect optimizer statistics")
})

test_that("translate sql server -> Redshift analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "ANALYZE results_schema.heracles_results;")
})

test_that("translate sql server -> Postgres analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "ANALYZE results_schema.heracles_results;")
})

test_that("translate sql server -> Impala analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
                      targetDialect = "impala")
  expect_equal_ignore_spaces(sql, "COMPUTE STATS results_schema.heracles_results;")
})

test_that("translate sql server -> Netezza analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
                      targetDialect = "netezza")
  expect_equal_ignore_spaces(sql, "GENERATE STATISTICS ON results_schema.heracles_results;")
})

test_that("translate sql server -> Postgres DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);",
                      targetDialect = "postgresql")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP, b TIMESTAMP);")
})

test_that("translate sql server -> Oracle DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP, b TIMESTAMP);")
})

test_that("translate sql server -> redshift DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);",
                      targetDialect = "redshift")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x  (a TIMESTAMP, b TIMESTAMP)\nDISTSTYLE ALL;")
})

test_that("translateSingleStatement sql server -> oracle with trailing ;", {
  sql <- translateSingleStatement("SELECT * FROM my_table;",
                      targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table ")
})

test_that("translateSingleStatement sql server -> oracle with trailing ; but BEGIN END", {
  sql <- translateSingleStatement("BEGIN\nSELECT * INTO a FROM b;\nEND;",
                                     targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "BEGIN \n CREATE TABLE a AS \n SELECT \n * \n FROM \n b ; \n END;")
})

test_that("translateSingleStatement sql server -> oracle throw error if > 1 statement", {
  expect_error(
    sql <- translateSingleStatement("TRUNCATE my_table; DROP TABLE my_table;",
                                       targetDialect = "oracle")
  )
})

test_that("translate create table if not exists pdw", {
  sql <- translate( "IF OBJECT_ID('test.testing', 'U') IS NULL create table test.testing (id int);", 
                    targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "IF XACT_STATE() = 1 COMMIT; IF OBJECT_ID('test.testing', 'U') IS NULL  CREATE TABLE  test.testing  (id int)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translate SELECT INTO + CTE bigquery", {
  sql <- translate( "WITH data (a,b) AS (SELECT 1, 2 UNION ALL SELECT 3, 4) SELECT a,b INTO test FROM data;",
                    targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "CREATE TABLE test AS WITH data as (select 1 as a, 2 as b union all select 3, 4) SELECT a,b FROM data;")
})

# Hive tests

test_that("translate sql server -> Hive clustered index is not supported", {
    sql <- translate("CREATE CLUSTERED INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "-- hive does not support indexes")
})

test_that("translate sql server -> Hive index is not supported", {
    sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "-- hive does not support indexes")
})

test_that("translate sql server -> Hive index with Where is not supported", {
    sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date) WHERE cohort_definition_id=1;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "-- hive does not support indexes")
})

test_that("translate sql server -> Hive CHARINDEX from position", {
    sql <- translate("SELECT CHARINDEX('test','abctest') FROM table",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT INSTR('abctest','test') FROM table")
})

test_that("translate sql server -> Hive COUNT", {
    sql <- translate("SELECT COUNT_BIG('test') FROM table",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT COUNT('test') FROM table")
})

test_that("translate sql server -> Hive left SUBSTR", {
    sql <- translate("SELECT LEFT('test',3)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT SUBSTR('test',1,3)")
})

test_that("translate sql server -> Hive right SUBSTR", {
    sql <- translate("SELECT RIGHT('test',3)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT SUBSTR('test',-3)")
})

test_that("translate sql server -> Hive LENGTH", {
    sql <- translate("SELECT LEN('test')",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT LENGTH('test')")
})

test_that("translate sql server -> Hive LN", {
    sql <- translate("SELECT LOG(10)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT LN(10)")
})

test_that("translate sql server -> Hive new ID", {
    sql <- translate("SELECT NEWID()",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT reflect('java.util.UUID','randomUUID')")
})

test_that("translate sql server -> Hive ROUND", {
    sql <- translate("SELECT ROUND('100.2564', 2)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT ROUND(CAST('100.2564' AS DOUBLE),2)")
})

test_that("translate sql server -> Hive SQUARE", {
    sql <- translate("SELECT SQUARE(2)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT ((2)*(2))")
})

test_that("translate sql server -> Hive STDDEV", {
    sql <- translate("SELECT STDEV(4)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT STDDEV_POP(4)")
})

test_that("translate sql server -> Hive VARIANCE", {
    sql <- translate("SELECT VAR(4)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT VARIANCE(4)")
})

test_that("translate sql server -> Hive DATE_ADD day", {
    sql <- translate("SELECT DATEADD(d,30,CAST(drug_era_end_date AS DATE)) FROM drug_era;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT DATE_ADD(drug_era_end_date, 30) FROM drug_era;")
})

test_that("translate sql server -> Hive DATE_ADD month", {
    sql <- translate("SELECT DATEADD(month,3,CAST(drug_era_end_date AS DATE)) FROM drug_era;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT CAST(ADD_MONTHS(drug_era_end_date, 3) AS TIMESTAMP) FROM drug_era;")
})

test_that("translate sql server -> Hive DATEFROMPARTS", {
    sql <- translate("SELECT DATEFROMPARTS(1999,12,12);",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT CAST(CONCAT(CAST(1999 AS STRING),'-',CAST(12 AS STRING),'-',CAST(12 AS STRING)) AS TIMESTAMP);")
})

test_that("translate sql server -> Hive EOMONTH", {
    sql <- translate("SELECT eomonth(drug_era_end_date);",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT CAST(last_day(drug_era_end_date) AS TIMESTAMP);")
})

test_that("translate sql server -> Hive TIMESTAMP", {
    sql <- translate("SELECT GETDATE();",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT unix_timestamp();")
})

test_that("translate sql server -> Hive Year TIMESTAMP", {
    sql <- translate("SELECT year(unix_timestamp());",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT year(from_unixtime(unix_timestamp()));")
})

test_that("translate sql server -> Hive CREATE TABLE", {
    sql <- translate("IF OBJECT_ID('test.testing', 'U') IS NULL CREATE TABLE test.testing (id int);",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS test.testing (id int);")
})

test_that("translate sql server -> Hive DROP TABLE", {
    sql <- translate("IF OBJECT_ID('test.testing', 'U') IS NOT NULL DROP TABLE test.testing;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test.testing;")
})

test_that("translate sql server -> Hive UNION", {
    sql <- translate("(SELECT test UNION SELECT ytest) ORDER BY",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT * FROM\n(SELECT test\nUNION\nSELECT ytest)\nAS t1 ORDER BY")
})

test_that("translate sql server -> Hive PARTITION IF NOT EXISTS", {
    sql <- translate("HINT PARTITION(cohort_definition_id)
    IF OBJECT_ID('@results_schema.heracles_results_dist', 'U') IS NULL
CREATE TABLE heracles_results_dist
(
	cohort_definition_id int,
	analysis_id int,
	stratum_1 varchar(255),
);",
    targetDialect = "hive")
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
);",
    targetDialect = "hive")
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
);",
    targetDialect = "hive")
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
);",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "table with bucket
    CREATE TABLE heracles_results_dist
(cohort_definition_id int,
	analysis_id int,
	stratum_1 varchar(255),
)
CLUSTERED by(analysis_id) into 64 BUCKETS;")
})

test_that("translate sql server -> Hive dbo", {
    sql <- translate(".dbo.",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, ".")
})

test_that("translate sql server -> Hive TOP in subqueries", {
    sql <- translate("select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "select statistic_value from achilles_results join (SELECT count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003)")
})

test_that("translate sql server -> Hive TOP in subqueries with parentheses", {
    sql <- translate("(select statistic_value from achilles_results join (SELECT TOP 1 count as total_pts from achilles_results where analysis_id = 1) where analysis_id in (2002,2003))",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "(select statistic_value from achilles_results join (SELECT count as total_pts from achilles_results where analysis_id = 1 LIMIT 1) where analysis_id in (2002,2003))")
})

test_that("translate sql server -> Hive DATE", {
    sql <- translate("DATE",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive DATETIME", {
    sql <- translate("DATETIME",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive DATETIME2", {
    sql <- translate("DATETIME2",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive BIGINT NOT NULL", {
    sql <- translate("BIGINT NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "BIGINT")
})

test_that("translate sql server -> Hive BOOLEAN NOT NULL", {
    sql <- translate("BOOLEAN NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "BOOLEAN")
})

test_that("translate sql server -> Hive CHAR NOT NULL", {
    sql <- translate("CHAR NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CHAR")
})

test_that("translate sql server -> Hive DECIMAL NOT NULL", {
    sql <- translate("DECIMAL NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DECIMAL")
})

test_that("translate sql server -> Hive DOUBLE NOT NULL", {
    sql <- translate("DOUBLE NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DOUBLE")
})

test_that("translate sql server -> Hive FLOAT NOT NULL", {
    sql <- translate("FLOAT NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive INT NOT NULL", {
    sql <- translate("INT NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "INT")
})

test_that("translate sql server -> Hive REAL NOT NULL", {
    sql <- translate("REAL NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive SMALLINT NOT NULL", {
    sql <- translate("SMALLINT NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SMALLINT")
})

test_that("translate sql server -> Hive STRING NOT NULL", {
    sql <- translate("STRING NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "VARCHAR")
})

test_that("translate sql server -> Hive TIMESTAMP NOT NULL", {
    sql <- translate("TIMESTAMP NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive TINYINT NOT NULL", {
    sql <- translate("TINYINT NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TINYINT")
})

test_that("translate sql server -> Hive VARCHAR NOT NULL", {
    sql <- translate("VARCHAR(10) NOT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "VARCHAR(10)")
})

test_that("translate sql server -> Hive BIGINT NULL", {
    sql <- translate("BIGINT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "BIGINT")
})

test_that("translate sql server -> Hive BOOLEAN NULL", {
    sql <- translate("BOOLEAN NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "BOOLEAN")
})

test_that("translate sql server -> Hive CHAR NULL", {
    sql <- translate("CHAR NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CHAR")
})

test_that("translate sql server -> Hive DECIMAL NULL", {
    sql <- translate("DECIMAL NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DECIMAL")
})

test_that("translate sql server -> Hive DOUBLE NULL", {
    sql <- translate("DOUBLE NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DOUBLE")
})

test_that("translate sql server -> Hive FLOAT NULL", {
    sql <- translate("FLOAT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive INT NULL", {
    sql <- translate("INT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "INT")
})

test_that("translate sql server -> Hive REAL NULL", {
    sql <- translate("FLOAT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "FLOAT")
})

test_that("translate sql server -> Hive SMALLINT NULL", {
    sql <- translate("SMALLINT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SMALLINT")
})

test_that("translate sql server -> Hive STRING NULL", {
    sql <- translate("STRING NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "VARCHAR")
})

test_that("translate sql server -> Hive TIMESTAMP NULL", {
    sql <- translate("TIMESTAMP NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TIMESTAMP")
})

test_that("translate sql server -> Hive TINYINT NULL", {
    sql <- translate("TINYINT NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "TINYINT")
})

test_that("translate sql server -> Hive VARCHAR NULL", {
    sql <- translate("VARCHAR(10) NULL",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "VARCHAR(10)")
})

test_that("translate sql server -> Hive CHAR", {
    sql <- translate("CHAR,",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CHAR(1),")
})

test_that("translate sql server -> Hive CHAR\n", {
    sql <- translate("CHAR\n+",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CHAR(1)\n")
})

test_that("translate sql server -> Hive CHAR)", {
    sql <- translate("CHAR)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CHAR(1))")
})

test_that("translate sql server -> Hive CONSTRAINT DEFAULT timestamp", {
    sql <- translate("CONSTRAINT test DEFAULT unix_timestamp()",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Hive DEFAULT timestamp", {
    sql <- translate("DEFAULT unix_timestamp()",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Hive UPDATE STATISTICS", {
    sql <- translate("UPDATE STATISTICS results_schema.heracles_results;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "-- hive does not support COMPUTE STATS")
})

test_that("translate sql server -> Hive CAST VARCHAR", {
    sql <- translate("CAST(10 AS VARCHAR)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CAST(10 AS VARCHAR(1000))")
})

test_that("translate sql server -> Hive COALESCE", {
    sql <- translate("ISNULL(abc,gde)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "COALESCE(abc,gde)")
})

test_that("translate sql server -> Hive WITH AS temp", {
    sql <- translate("WITH cteRawData as (select coh_id FROM #raw_706),
overallStats as (select coh_id from cteRawData),
valueStats as (select total FROM (select coh_id FROM cteRawData GROUP BY coh_id) D)
select o.coh_id, 706 as analysis_id into #results_dist_706 from valueStats s
join overallStats o on s.coh_id = o.coh_id;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS cteRawData; DROP TABLE IF EXISTS overallStats; DROP TABLE IF EXISTS valueStats;
    
    CREATE TEMPORARY TABLE cteRawData AS select coh_id FROM raw_706;
    CREATE TEMPORARY TABLE overallStats AS select coh_id from cteRawData;
    CREATE TEMPORARY TABLE valueStats AS select total FROM (select coh_id FROM cteRawData GROUP BY coh_id) D;
    CREATE TEMPORARY TABLE results_dist_706 AS SELECT o.coh_id, 706 as analysis_id FROM valueStats s
    join overallStats o on s.coh_id = o.coh_id;")
})

test_that("translate sql server -> Hive TEMP TABLE", {
    sql <- translate("select coh_id into #raw_706 from cteRawData;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TEMPORARY TABLE IF NOT EXISTS raw_706 AS
    SELECT
    coh_id
    FROM
    cteRawData;")
})

test_that("translate sql server -> Hive TEMP TABLE without from", {
    sql <- translate("select coh_id into #raw_706;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TEMPORARY TABLE IF NOT EXISTS raw_706 AS
    SELECT
    coh_id;")
})

test_that("translate sql server -> Hive TEMP TABLE if not exists", {
    sql <- translate("CREATE TABLE #raw_706 (coh_id int)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TEMPORARY TABLE IF NOT EXISTS raw_706 (coh_id int)")
})

test_that("translate sql server -> Hive several TEMP TABLE", {
    sql <- translate("CREATE TEMPORARY TABLE raw_707 as (select coh_id FROM #raw_706), overallStats (coh_id) as (select coh_id from cteRawData)
    ;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS raw_707; DROP TABLE IF EXISTS overallStats; CREATE TEMPORARY TABLE raw_707 AS (select coh_id FROM raw_706)
     ;
     CREATE TEMPORARY TABLE overallStats AS (select coh_id from cteRawData)
    ;")
})

test_that("translate sql server -> Hive several TEMP TABLE without definitions", {
    sql <- translate("CREATE TEMPORARY TABLE raw_707 as (select coh_id FROM #raw_706), overallStats as (select coh_id from cteRawData)
    ;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS raw_707; DROP TABLE IF EXISTS overallStats; CREATE TEMPORARY TABLE raw_707 AS (select coh_id FROM raw_706)
     ;
     CREATE TEMPORARY TABLE overallStats AS (select coh_id from cteRawData)
    ;")
})

test_that("translate sql server -> Hive DROP with definition", {
    sql <- translate("DROP TABLE IF EXISTS test.testing (id int)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test.testing ")
})

test_that("translate sql server -> Hive Subquery", {
    sql <- translate("SELECT o.coh_id, 706 as analysis_id into results_dist_706 from valueStats;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS results_dist_706 AS
    SELECT
    o.coh_id, 706 as analysis_id
    FROM
    valueStats;")
})

test_that("translate sql server -> Hive DISTINCT", {
    sql <- translate("SELECT o.coh_id, 706 as analysis_id into results_dist_706 from valueStats;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS results_dist_706 AS
    SELECT
    o.coh_id, 706 as analysis_id
    FROM
    valueStats;")
})

test_that("translate sql server -> Hive intersect distinct", {
    sql <- translate("SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s;",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2;")
})

test_that("translate sql server -> Hive bracketed intersect distinct", {
    sql <- translate("(SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s)",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "(SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2)")
})

test_that("translate sql server -> Hive Dash", {
    sql <- translate("#",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> Hive extra space", {
    sql <- translate("(coh_id int, analysis_id int)  AS select o.coh_id, 706 as analysis_id FROM valueStats s",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "(coh_id int, analysis_id int) AS select o.coh_id, 706 as analysis_id FROM valueStats s")
})

test_that("translate sql server -> Hive table without definition", {
    sql <- translate("CREATE TABLE cteRawData (coh_id int) AS select coh_id FROM raw_706",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "CREATE TABLE cteRawData AS select coh_id FROM raw_706")
})

test_that("translate sql server -> Hive digits", {
    sql <- translate("WHEN .123456 * ",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "WHEN 0.123456 * ")
})

test_that("translate sql server -> Hive digits", {
    sql <- translate("WHEN .123456 * ",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "WHEN 0.123456 * ")
})

test_that("translate sql server -> Hive ISNUMERIC", {
    sql <- translate("select ISNUMERIC(a) from b", 
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "select case when cast(a as double) is not null then 1 else 0 end from b")
})

test_that("translate sql server -> Hive AS", {
    sql <- translate("as \"test_variable\"",
    targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "as test_variable")
})

test_that("translate sql server -> Hive HASHBYTES", {
    sql <- translate("SELECT AVG(CAST(CAST(CONVERT(VARBINARY, HASHBYTES('MD5',line), 1) AS INT) AS BIGINT)) as checksum",
      targetDialect = "hive")
    expect_equal_ignore_spaces(sql, "SELECT AVG(CAST(CAST(hash(line) AS INT) AS BIGINT)) as checksum")
})
