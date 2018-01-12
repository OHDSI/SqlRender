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

test_that("translateSQL sql server -> Oracle DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})


test_that("translateSQL sql server -> Oracle DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT (drug_era_end_date + NUMTODSINTERVAL(30, 'day')) FROM drug_era;")
})

test_that("translateSQL sql server -> Oracle USE", {
  sql <- translateSql("USE vocabulary;", targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "ALTER SESSION SET current_schema = vocabulary;")
})

test_that("translateSQL sql server -> Oracle DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "BEGIN\n EXECUTE IMMEDIATE 'TRUNCATE TABLE cohort';\n EXECUTE IMMEDIATE 'DROP TABLE cohort';\nEXCEPTION\n WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;")
})


test_that("translateSQL sql server -> Oracle CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "CAST('20000101' AS DATE);")
})

test_that("translateSQL sql server -> Oracle CONVERT(AS DATE)", {
  sql <- translateSql("CONVERT(DATE, '20000101');",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101', 'YYYYMMDD');")
})

test_that("translateSQL sql server -> Oracle concatenate string operator", {
  sql <- translateSql("select distinct CONVERT(DATE, cast(YEAR(observation_period_start_date) as varchar(4)) + '01' + '01') as obs_year from observation_period;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT distinct TO_DATE(cast(EXTRACT(YEAR FROM observation_period_start_date) as varchar(4)) || '01' || '01', 'YYYYMMDD') as obs_year  FROM observation_period ;")
})

test_that("translateSQL sql server -> Oracle RIGHT functions", {
  sql <- translateSql("select RIGHT(x,4);",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4) FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle complex query", {
  sql <- translateSql("select CONVERT(DATE,CAST(YEAR(DATEFROMPARTS(2000,1,1)) AS VARCHAR(12)) + RIGHT('0'+MONTH(DATEFROMPARTS(2000,1,1)),2) + '01') as X;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT TO_DATE(CAST(EXTRACT(YEAR FROM TO_DATE(TO_CHAR(2000,'0000')||'-'||TO_CHAR(1,'00')||'-'||TO_CHAR(1,'00'), 'YYYY-MM-DD'))  AS varchar(12)) || SUBSTR('0' ||EXTRACT(MONTH FROM TO_DATE(TO_CHAR(2000,'0000')||'-'||TO_CHAR(1,'00')||'-'||TO_CHAR(1,'00'), 'YYYY-MM-DD')),-2) || '01', 'YYYYMMDD') as X FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle '+' in quote", {
  sql <- translateSql("select '+';", targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT '+' FROM DUAL;")
})

test_that("translateSQL sql server -> PostgreSQL USE", {
  sql <- translateSql("USE vocabulary;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SET search_path TO vocabulary;")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("'x' + b ( 'x' + b)",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "'x' || b ( 'x' || b)")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';b'", targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "a || ';b'")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';('", targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "a || ';('")
})


test_that("translateSQL sql server -> PostgreSQL add month", {
  sql <- translateSql("DATEADD(mm,1,date)",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "(date + 1*INTERVAL'1 month')")
})

test_that("translateSQL sql server -> Oracle multiple inserts in one statement", {
  sql <- translateSql("INSERT INTO my_table (key,value) VALUES (1,0),(2,0),(3,1)",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "INSERT ALL\nINTO my_table   (key,value) VALUES (1,0)\n INTO my_table  (key,value) VALUES (2,0)\n)\n INTO my_table   (key,value) VALUES (3,1)\nSELECT * FROM dual")
})

test_that("translateSQL sql server -> RedShift VARCHAR(MAX)", {
  sql <- translateSql("VARCHAR(MAX)", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "VARCHAR(MAX)")
})

test_that("translateSQL sql server -> Postgres WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO without FROM", {
  sql <- translateSql("SELECT c INTO d;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE d AS\nSELECT\nc ;")
})


test_that("translateSQL sql server -> Postgres WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translateSQL sql server -> Oracle WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "INSERT INTO c (d int)  WITH cte1 AS (SELECT a FROM b)  SELECT e FROM cte1;")
})

test_that("translateSQL sql server -> PDW WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE d WITH (DISTRIBUTION = REPLICATE)\nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translateSQL sql server -> PDW WITH SELECT INTO temp table", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO #d FROM cte1;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #d   WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;")
})

test_that("translateSQL sql server -> PDW create temp table", {
  sql <- translateSql("CREATE TABLE #a (x int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a (x int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> PDW create temp table with person_id", {
  sql <- translateSql("CREATE TABLE #a (person_id int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( person_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> PDW create temp table with subject_id", {
  sql <- translateSql("CREATE TABLE #a (subject_id int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( subject_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(subject_id));")
})

test_that("translateSQL sql server -> PDW create temp table with analysis_id", {
  sql <- translateSql("CREATE TABLE #a (analysis_id int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( analysis_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(analysis_id));")
})

test_that("translateSQL sql server -> PDW create permanent table", {
  sql <- translateSql("CREATE TABLE a (x int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (x int)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> PDW create permanent table with person_id", {
  sql <- translateSql("CREATE TABLE a (person_id int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( person_id int)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> PDW create permanent table with subject_id", {
  sql <- translateSql("CREATE TABLE a (subject_id int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( subject_id int)\nWITH (DISTRIBUTION = HASH(subject_id));")
})

test_that("translateSQL sql server -> PDW create permanent table with analysis_id", {
  sql <- translateSql("CREATE TABLE a (analysis_id int);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( analysis_id int)\nWITH (DISTRIBUTION = HASH(analysis_id));")
})

test_that("translateSQL sql server -> PDW select into permanent table", {
  sql <- translateSql("SELECT a INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n a \nFROM\n c WHERE a = 1;")
})

test_that("translateSQL sql server -> PDW select into permanent table with person_id", {
  sql <- translateSql("SELECT a, person_id, b INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = HASH(person_id))\nAS\nSELECT\n a, person_id, b \nFROM\n c WHERE a = 1;")
})

test_that("translateSQL sql server -> PDW select into permanent table with analysis_id", {
  sql <- translateSql("SELECT a, analysis_id, b INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = HASH(analysis_id))\nAS\nSELECT\n a, analysis_id, b \nFROM\n c WHERE a = 1;")
})

test_that("translateSQL sql server -> Postgres create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Redshift create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE  IF NOT EXISTS  cohort\n  (cohort_definition_id INT)\nDISTSTYLE ALL;")
})

test_that("translateSQL sql server -> Oracle create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "BEGIN\n EXECUTE IMMEDIATE 'CREATE TABLE cohort\n (cohort_definition_id INT)';\nEXCEPTION\n WHEN OTHERS THEN\n IF SQLCODE != -955 THEN\n RAISE;\n END IF;\nEND;")
})

test_that("translateSQL sql server -> Oracle datefromparts", {
  sql <- translateSql("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), 'YYYY-MM-DD') FROM table")
})

test_that("translateSQL sql server -> redshift datefromparts", {
  sql <- translateSql("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT TO_DATE(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM'), 'YYYY-MM-DD') FROM table")
})


test_that("translateSQL sql server -> Oracle datetime to timestamp", {
  sql <- translateSql("CREATE TABLE x (a DATETIME)",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP)")
})

test_that("translateSQL sql server -> Oracle select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_RANDOM.VALUE) AS rn FROM table ) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Postgres select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Redshift select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Oracle select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_CRYPTO.HASH(TO_CHAR(person_id ),2)) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Postgres select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Redshift select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> PDW cte with preceding 'with' in quotes", {
  sql <- translateSql("insert into x (a) values ('with'); with cte (a) as(select a from b) select a INTO #c from cte;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql,
                             "insert into x (a) values ('with'); IF XACT_STATE() = 1 COMMIT; CREATE TABLE #c WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte (a) AS (select a from b) SELECT\n a \nFROM\n cte;")
})

test_that("translateSQL sql server throws error when invalid target is given", {
  expect_error(translateSql("iSELECT * FROM a;", targetDialect = "pwd")$sql)
})


test_that("translateSQL select into issue for pdw", {
  sql <- "SELECT @c1 INTO table FROM @c2 WHERE a = 1;"
  sql <- translateSql(sql, targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE table WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n @c1 \nFROM\n @c2 WHERE a = 1;")
})



test_that("translateSQL ## issue on oracle", {
  sql <- "SELECT a FROM c##blah.table;"
  sql <- translateSql(sql, targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT a FROM c##blah.table;")
})

# Impala tests

test_that("translateSQL sql server -> Impala USE", {
  sql <- translateSql("USE vocabulary;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "USE vocabulary;")
})

test_that("translateSQL sql server -> Impala CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "CASE TYPEOF('20000101' ) WHEN 'TIMESTAMP' THEN CAST('20000101'  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST('20000101'  AS STRING), 1, 4), SUBSTR(CAST('20000101'  AS STRING), 5, 2), SUBSTR(CAST('20000101'  AS STRING), 7, 2)), 'UTC') END;")
})

test_that("translateSQL sql server -> Impala DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, CASE TYPEOF(drug_era_start_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_start_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_start_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_start_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_start_date  AS STRING), 7, 2)), 'UTC') END) FROM drug_era;")
})

test_that("translateSQL sql server -> Impala DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATE_ADD(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Impala WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Impala WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> Impala WITH SELECT INTO without FROM", {
  sql <- translateSql("SELECT c INTO d;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE d AS\nSELECT\n c;")
})

test_that("translateSQL sql server -> Impala create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Impala DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql,
                             "DROP TABLE IF EXISTS cohort;")
})

test_that("translateSQL sql server -> Impala UNION ORDER BY", {
  sql <- translateSql("(SELECT a FROM b UNION SELECT a FROM c) ORDER BY a",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql,
                             "SELECT * FROM \n ( SELECT a FROM b \n UNION \n SELECT a FROM c ) \n AS t1 ORDER BY a")
})

test_that("translateSQL sql server -> Impala RIGHT functions", {
  sql <- translateSql("SELECT RIGHT(x,4);",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4);")
})

test_that("translateSQL sql server -> Impala DELETE FROM", {
  sql <- translateSql("delete from ACHILLES_results;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "TRUNCATE TABLE ACHILLES_results;")
})

test_that("translateSQL sql server -> Impala DELETE FROM WHERE", {
  sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "INSERT OVERWRITE TABLE ACHILLES_results SELECT * FROM ACHILLES_results WHERE NOT(analysis_id IN (1, 2, 3));")
})

test_that("translateSQL sql server -> Impala location reserved word", {
  sql <- translateSql("select count(1) from omop_cdm.location;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "select count(1) from omop_cdm.`location`;")
})

test_that("translateSQL sql server -> Impala CREATE TABLE with NOT NULL", {
  sql <- translateSql("CREATE TABLE a (c1 BIGINT NOT NULL, c2 BOOLEAN NOT NULL, c3 CHAR NOT NULL, c4 DECIMAL NOT NULL, c5 DOUBLE NOT NULL, c6 FLOAT NOT NULL, c7 INT NOT NULL, c8 REAL NOT NULL, c9 SMALLINT NOT NULL, c10 STRING NOT NULL, c11 TIMESTAMP NOT NULL, c12 TINYINT NOT NULL, c13 VARCHAR(10) NOT NULL, c14 DATE NOT NULL, c15 DATETIME NOT NULL)",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 BIGINT, c2 BOOLEAN, c3 CHAR(1), c4 DECIMAL, c5 DOUBLE, c6 FLOAT, c7 INT, c8 REAL, c9 SMALLINT, c10 STRING, c11 TIMESTAMP, c12 TINYINT, c13 VARCHAR(10), c14 TIMESTAMP, c15 TIMESTAMP)")
})

test_that("translateSQL sql server -> Impala CREATE TABLE with NULL", {
    sql <- translateSql("CREATE TABLE a (c1 BIGINT NULL, c2 BOOLEAN NULL, c3 CHAR NULL, c4 DECIMAL NULL, c5 DOUBLE NULL, c6 FLOAT NULL, c7 INT NULL, c8 REAL NULL, c9 SMALLINT NULL, c10 STRING NULL, c11 TIMESTAMP NULL, c12 TINYINT NULL, c13 VARCHAR(10) NULL, c14 DATE NULL, c15 DATETIME NULL)",
                    targetDialect = "impala")$sql
    expect_equal_ignore_spaces(sql, "CREATE TABLE a (c1 BIGINT, c2 BOOLEAN, c3 CHAR(1), c4 DECIMAL, c5 DOUBLE, c6 FLOAT, c7 INT, c8 REAL, c9 SMALLINT, c10 STRING, c11 TIMESTAMP, c12 TINYINT, c13 VARCHAR(10), c14 TIMESTAMP, c15 TIMESTAMP)")
})

test_that("translateSQL sql server -> Impala clause with NOT NULL", {
  sql <- translateSql("SELECT * FROM x WHERE y IS NOT NULL",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM x WHERE y IS NOT NULL")
})

test_that("translateSQL sql server -> Impala CREATE TABLE with CONSTRAINT DEFAULT", {
    sql <- translateSql("CREATE TABLE a(c1 TIMESTAMP CONSTRAINT a_c1_def DEFAULT NOW())",
                    targetDialect = "impala")$sql
    expect_equal_ignore_spaces(sql, "CREATE TABLE a(c1 TIMESTAMP)")
})

test_that("translateSQL sql server -> Impala stats reserved word",{
    sql <- translateSql("SELECT * FROM strata_stats AS stats",
                        targetDialect = "impala")$sql
    expect_equal_ignore_spaces(sql, "SELECT * FROM strata_stats AS _stats")
})


# Netezza tests

test_that("translateSQL sql server -> Netezza CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101' , 'yyyymmdd');")
})

test_that("translateSQL sql server -> Netezza DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translateSQL sql server -> Netezza DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Netezza WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Netezza WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql,
                             "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b) SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> Netezza DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql,
                             "DROP TABLE cohort IF EXISTS;")
})

test_that("translateSQL sql server -> Netezza RIGHT functions", {
  sql <- translateSql("SELECT RIGHT(x,4);",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "SELECT STRRIGHT(x,4);")
})

test_that("translateSQL sql server -> Netezza DELETE FROM WHERE", {
  sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "delete from ACHILLES_results where analysis_id IN (1, 2, 3);")
})

test_that("translateSQL sql server -> Netezza CAST AS VARCHAR", {
  sql <- translateSql("CAST(person_id AS VARCHAR);",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "CAST(person_id AS VARCHAR(1000));")
})

test_that("translateSQL sql server -> PostgreSql TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> Oracle TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b AND ROWNUM <= 10; ")
})

test_that("translateSQL sql server -> impala TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "impala")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> redshift TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "netezza")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> postgres date to varchar", {
  sql <- translateSql("SELECT CONVERT(VARCHAR,start_date,112) FROM table;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SELECT TO_CHAR(start_date, 'YYYYMMDD') FROM table;")
})


test_that("translateSQL sql server -> pdw hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> pdw hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE(row_id INT);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE (row_id INT)\nWITH (DISTRIBUTION = HASH(row_id));")
})


test_that("translateSQL sql server -> redshift hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE  #my_table\nDISTKEY(row_id)\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> redshift hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE my_table (row_id INT);",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE my_table (row_id INT)\nDISTKEY(row_id);")
})

test_that("translateSql: warning on temp table name that is too long", {
  expect_warning(translateSql("SELECT * FROM #abcdefghijklmnopqrstuvwxyz", "pdw")$sql)
})

test_that("translateSql: warning on table name that is too long", {
  expect_warning(translateSql("DROP TABLE abcdefghijklmnopqrstuvwxyz123456789", "pdw")$sql)
})


test_that("translateSQL sql server -> oracle concat", {
  sql <- translateSql("SELECT CONCAT(a,\" , \",c,d,e) FROM x;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT CONCAT(a, CONCAT(\" , \", CONCAT( c, CONCAT( d, e)))) FROM x;")
})

test_that("translateSQL sql server -> oracle natural log", {
  sql <- translateSql("SELECT LOG(number) FROM table",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT LOG(2.718281828459,number) FROM table")
})

test_that("translateSQL sql server -> oracle log base 10", {
  sql <- translateSql("SELECT LOG10(number) FROM table;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT LOG(10,number) FROM table;")
})

test_that("translateSQL sql server -> oracle log any base", {
  sql <- translateSql("SELECT LOG(number, base) FROM table",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT LOG( base,number) FROM table")
})

test_that("translateSQL sql server -> redshift natural log", {
  sql <- translateSql("SELECT LOG(number) FROM table",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((number) AS REAL)) FROM table")
})

test_that("translateSQL sql server -> redshift log base 10", {
  sql <- translateSql("SELECT LOG10(number) FROM table;",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT LOG(CAST((number) AS REAL)) FROM table;")
})

test_that("translateSQL sql server -> redshift log any base", {
  sql <- translateSql("SELECT LOG(number, base) FROM table",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT (LN(CAST((number) AS REAL))/LN(CAST(( base) AS REAL))) FROM table")
})

test_that("translateSQL sql server -> postgresql natural log", {
  sql <- translateSql("SELECT LOG(number) FROM table",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SELECT LN(CAST((number) AS REAL)) FROM table")
})

test_that("translateSQL sql server -> postgresql log base 10", {
  sql <- translateSql("SELECT LOG10(number) FROM table;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SELECT LOG(10,CAST((number) AS NUMERIC)) FROM table;")
})

test_that("translateSQL sql server -> postgresql log any base", {
  sql <- translateSql("SELECT LOG(number, base) FROM table",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SELECT LOG(CAST(( base) AS NUMERIC),CAST((number) AS NUMERIC)) FROM table")
})

test_that("translateSQL sql server -> oracle union 1", {
  sql <- translateSql("SELECT * FROM table1 WHERE a = 1 UNION SELECT * FROM table2 WHERE a = 1;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM table1 WHERE a = 1 UNION SELECT * FROM table2 WHERE a = 1;")
})

test_that("translateSQL sql server -> oracle union 2", {
  sql <- translateSql("SELECT * FROM table1 UNION SELECT * FROM table2 WHERE a = 1;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT * FROM table1 UNION SELECT * FROM table2 WHERE a = 1;")
})

test_that("translateSQL sql server -> oracle from dual", {
  sql <- translateSql("SELECT 1 AS id;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT 1 AS id FROM DUAL;")
})


test_that("translateSQL sql server -> oracle from dual", {
  sql <- translateSql("SELECT (SELECT id FROM a WHERE b=2) AS id;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT (SELECT id FROM a WHERE b=2) AS id FROM DUAL;")
})

test_that("translateSQL sql server -> oracle ISNUMERIC", {
  sql <- translateSql("SELECT CASE WHEN ISNUMERIC(a) THEN a ELSE b FROM c;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN a ELSE b  FROM c ;")
})

test_that("translateSQL sql server -> postgres ISNUMERIC", {
  sql <- translateSql("SELECT CASE WHEN ISNUMERIC(a) THEN a ELSE b FROM c;",
                      targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN (a ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN a ELSE b FROM c;")
})

test_that("translateSQL sql server -> bigquery lowercase all but strings", {
  sql <- translateSql("SELECT X.Y, 'Mixed Case String' FROM \"MixedCaseTableName.T\" GROUP BY X.Y",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select x.y, 'Mixed Case String' from \"MixedCaseTableName.T\" group by x.y ")
})

test_that("translateSQL sql server -> bigquery common table expression column list", {
  sql <- translateSql("with cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "with cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;")
})

test_that("translateSQL sql server -> bigquery multiple common table expression column list", {
  sql <- translateSql("with cte1 as (select 2), cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "with cte1 as (select 2), cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;")
})

test_that("translateSQL sql server -> bigquery group by function", {
  sql <- translateSql("select f(a), count(*) from t group by f(a);",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select f(a), count(*) from t group by 1;")
})

test_that("translateSQL sql server -> bigquery group by addition", {
  sql <- translateSql("select 100, sum(x), cast(a+b as string) from t group by a+b;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select 100, sum(x), cast(a+b as string) from t group by 3;")
})

test_that("translateSQL sql server -> bigquery column ref groupby", {
  sql <- translateSql("select 100, sum(x), cast(a+b as string) from t group by t.a, t.b;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select 100, sum(x), cast(a+b as string) from t group by t.a, t.b;")
})

test_that("translateSQL sql server -> bigquery group by without match", {
  sql <- translateSql("select 100, sum(x), concat('count = ', c) from t group by a+b;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select 100, sum(x), concat('count = ', c) from t group by a + b;")
})

test_that("translateSQL sql server -> bigquery group by without final semicolon", {
  sql <- translateSql("select f(a) from t group by f(a)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select f(a) from t group by 1 ")
})

test_that("translateSQL sql server -> bigquery order by", {
  sql <- translateSql("select f(a) from t group by f(a) order by f(a);",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select f(a) from t group by 1 order by 1;")
})

test_that("translateSQL sql server -> bigquery nested group by", {
  sql <- translateSql("select * from (select 100, cast(a+b as string), max(x) from t group by a+b) dt;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select * from (select 100, cast(a+b as string), max(x) from t group by 2) dt;")
})

test_that("translateSQL sql server -> bigquery complex group by", {
  sql <- translateSql("select 100, 200, cast(floor(date_diff(a, b, day)/30) string string), 300 from t group by floor(date_diff(a, b, day)/30);",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select 100, 200, cast(floor(date_diff(a, b, day)/30) string string), 300 from t group by 3;")
})

test_that("translateSQL sql server -> bigquery column references", {
  sql <- translateSql("select concat(t.a, t.b) from t group by t.a, t.b;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(t.a, t.b) from t group by t.a, t.b;")
})

test_that("translateSQL sql server -> bigquery mixed column references", {
  sql <- translateSql("select concat(t.a, t.b), x+y+z from t group by t.a, t.b, x+y+z;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(t.a, t.b), x+y+z from t group by t.a, t.b, 2;")
})

test_that("translateSQL sql server -> bigquery CONCAT leading string", {
  sql <- translateSql("select 'a' + b + c from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(concat('a', b), c) from t;")
})

test_that("translateSQL sql server -> bigquery CONCAT leading cast", {
  sql <- translateSql("select cast(a as varchar) + b + c from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(concat(cast(a as string), b), c) from t;")
})

test_that("translateSQL sql server -> bigquery CONCAT leading isnull", {
  sql <- translateSql("select isnull(a, 'b') + c from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(IFNULL(a,'b'), c) from  t;")
})

test_that("translateSQL sql server -> bigquery CONCAT leading case", {
  sql <- translateSql("select case when x like '1' then 'a' when x like '2' then 'b' else 'c' end + c from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(case when x like '1' then 'a' when x like '2' then 'b' else 'c' end, c ) from  t;")
})

test_that("translateSQL sql server -> bigquery CONCAT second string", {
  sql <- translateSql("select a.b + 'c' from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat(a.b , 'c') from  t;")
})

test_that("translateSQL sql server -> bigquery CONCAT with alias", {
  sql <- translateSql("select 'a' + b AS concept_path from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat('a' , b ) as concept_path from  t;")
})

test_that("translateSQL sql server -> bigquery CONCAT with alias but no AS", {
  sql <- translateSql("select 'a' + b concept_path from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select concat('a' , b ) as concept_path from  t;")
})

test_that("translateSQL sql server -> bigquery DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select DATE_DIFF(cast(drug_era_end_date as date), cast(drug_era_start_date as date), DAY) from drug_era;")
})

test_that("translateSQL sql server -> bigquery DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select DATE_ADD(cast(drug_era_end_date as date), interval 30 DAY) from drug_era;")
})

test_that("translateSQL sql server -> bigquery GETDATE", {
  sql <- translateSql("GETDATE()",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "CURRENT_DATE()")
})

test_that("translateSQL sql server -> bigquery STDEV", {
  sql <- translateSql("stdev(x)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "STDDEV(x)")
})

test_that("translateSQL sql server -> bigquery LEN", {
  sql <- translateSql("len('abc')",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "LENGTH('abc')")
})

test_that("translateSQL sql server -> bigquery COUNT_BIG", {
  sql <- translateSql("COUNT_BIG(x)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "COUNT(x)")
})

test_that("translateSQL sql server -> bigquery CAST :string", {
  sql <- translateSql("select cast(x as:string)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select CAST(x as string)")
})

test_that("translateSQL sql server -> bigquery CAST :integer", {
  sql <- translateSql("select cast(x as:integer)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select CAST(x as int64)")
})

test_that("translateSQL sql server -> bigquery CAST varchar", {
  sql <- translateSql("select cast(x as varchar)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select cast(x as string)")
})

test_that("translateSQL sql server -> bigquery CAST :float", {
  sql <- translateSql("select cast(x as:float)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select CAST(x as float64)")
})

test_that("translateSQL sql server -> bigquery DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS cohort;")
})

test_that("translateSQL sql server -> bigquery CAST string", {
  sql <- translateSql("CAST(x AS VARCHAR(255))",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "CAST(x AS STRING)")
})

test_that("translateSQL sql server -> bigquery common table expression select into", {
  sql <- translateSql("WITH cte as (select 1) select * INTO #t FROM t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "INTO temp.t WITH cte as (select 1) select * FROM t;")
})

test_that("translateSQL sql server -> bigquery select into", {
  sql <- translateSql("select * INTO #t FROM t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "INTO temp.t SELECT * FROM t;")
})

test_that("translateSQL sql server -> bigquery LEFT, RIGHT", {
  sql <- translateSql("select LEFT(a, 20), RIGHT(b, 30) FROM t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select SUBSTR(a, 0, 20), SUBSTR(b, -30) from t;")
})

test_that("translateSQL sql server -> bigquery cast float", {
  sql <- translateSql("cast(a as float)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "cast(a as float64)")
})

test_that("translateSQL sql server -> bigquery cast bigint", {
  sql <- translateSql("cast(a as bigint)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "cast(a as int64)")
})

test_that("translateSQL sql server -> bigquery cast int", {
  sql <- translateSql("cast(a as int)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "cast(a as int64)")
})

test_that("translateSQL sql server -> bigquery cast date", {
  sql <- translateSql("date(d)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "cast(d as date)")
})

test_that("translateSQL sql server -> bigquery cast string as date", {
  sql <- translateSql("cast(concat(a,b) as date)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "parse_date('%Y%m%d', concat(a,b))")
})

test_that("translateSQL sql server -> bigquery extract year", {
  sql <- translateSql("year(d)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "EXTRACT(YEAR from d)")
})

test_that("translateSQL sql server -> bigquery extract month", {
  sql <- translateSql("month(d)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "EXTRACT(MONTH from d)")
})

test_that("translateSQL sql server -> bigquery extract day", {
  sql <- translateSql("day(d)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "EXTRACT(DAY from d)")
})

test_that("translateSQL sql server -> bigquery union distinct", {
  sql <- translateSql("select 1 as x union select 2;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select 1 as x union distinct select 2;")
})

test_that("translateSQL sql server -> bigquery intersect distinct", {
  sql <- translateSql("SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2;")
})

test_that("translateSQL sql server -> bigquery bracketed intersect distinct", {
  sql <- translateSql("(SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s)",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "(SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2)")
})

test_that("translateSQL sql server -> bigquery isnull", {
  sql <- translateSql("SELECT isnull(x,y) from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select IFNULL(x,y) from t;")
})

test_that("translateSQL sql server -> bigquery unquote aliases", {
  sql <- translateSql("SELECT a as \"b\" from t;",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select a as b from t;")
})

test_that("translateSQL sql server -> bigquery non-id in list", {
  sql <- translateSql("select * from t where x in ('333','22','1')",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select * from t where x in ('333','22','1')")
})

test_that("translateSQL sql server -> bigquery non-integer in lists", {
  sql <- translateSql("select * from t where x_id in ('333','22','1a')",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select * from t where x_id in ('333','22','1a')")
})

test_that("translateSQL sql server -> bigquery unquote id in lists", {
  sql <- translateSql("select * from t where x_id in ('333','22','1')",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select * from t where x_id in (333,22,1)")
})

test_that("translateSQL sql server -> bigquery cast int in coalesce", {
  sql <- translateSql("select coalesce(x, 0), coalesce(12, y) from t",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select coalesce(cast(x as int64), 0), coalesce(12, cast(y as int64)) from  t")
})

test_that("translateSQL sql server -> bigquery cast decimal", {
  sql <- translateSql("select cast(x as decimal(18,4)) from t",
                      targetDialect = "bigquery")$sql
  expect_equal_ignore_spaces(sql, "select cast(x as float64) from t")
})

test_that("translateSQL sql server -> RedShift DATEADD dd", {
  sql <- translateSql("SELECT DATEADD(dd, 30, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(day, CAST(30 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD mm", {
  sql <- translateSql("SELECT DATEADD(mm, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(month, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD m", {
  sql <- translateSql("SELECT DATEADD(m, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(month, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD yyyy", {
  sql <- translateSql("SELECT DATEADD(yyyy, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(year, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD yy", {
  sql <- translateSql("SELECT DATEADD(yy, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(year, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD qq", {
  sql <- translateSql("SELECT DATEADD(qq, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(quarter, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD q", {
  sql <- translateSql("SELECT DATEADD(q, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(quarter, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD wk", {
  sql <- translateSql("SELECT DATEADD(wk, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(week, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD ww", {
  sql <- translateSql("SELECT DATEADD(ww, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(week, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD hh", {
  sql <- translateSql("SELECT DATEADD(hh, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(hour, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD mi", {
  sql <- translateSql("SELECT DATEADD(mi, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(minute, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD ss", {
  sql <- translateSql("SELECT DATEADD(ss, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(second, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEADD mcs", {
  sql <- translateSql("SELECT DATEADD(mcs, 3, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEADD(microsecond, CAST(3 as int), drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF dd", {
  sql <- translateSql("SELECT DATEDIFF(dd, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF m", {
  sql <- translateSql("SELECT DATEDIFF(m, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF mm", {
  sql <- translateSql("SELECT DATEDIFF(mm, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF yyyy", {
  sql <- translateSql("SELECT DATEDIFF(yyyy, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF yy", {
  sql <- translateSql("SELECT DATEDIFF(yy, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF qq", {
  sql <- translateSql("SELECT DATEDIFF(qq, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF q", {
  sql <- translateSql("SELECT DATEDIFF(q, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF wk", {
  sql <- translateSql("SELECT DATEDIFF(wk, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF ww", {
  sql <- translateSql("SELECT DATEDIFF(ww, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF hh", {
  sql <- translateSql("SELECT DATEDIFF(hh, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF mi", {
  sql <- translateSql("SELECT DATEDIFF(mi, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF n", {
  sql <- translateSql("SELECT DATEDIFF(n, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF ss", {
  sql <- translateSql("SELECT DATEDIFF(ss, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF mcs", {
  sql <- translateSql("SELECT DATEDIFF(mcs, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(microsecond, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG dd", {
  sql <- translateSql("SELECT DATEDIFF_BIG(dd, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(day, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG m", {
  sql <- translateSql("SELECT DATEDIFF_BIG(m, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG mm", {
  sql <- translateSql("SELECT DATEDIFF_BIG(mm, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(month, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG yyyy", {
  sql <- translateSql("SELECT DATEDIFF_BIG(yyyy, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG yy", {
  sql <- translateSql("SELECT DATEDIFF_BIG(yy, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(year, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG qq", {
  sql <- translateSql("SELECT DATEDIFF_BIG(qq, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG q", {
  sql <- translateSql("SELECT DATEDIFF_BIG(q, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(quarter, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG wk", {
  sql <- translateSql("SELECT DATEDIFF_BIG(wk, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG ww", {
  sql <- translateSql("SELECT DATEDIFF_BIG(ww, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(week, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG hh", {
  sql <- translateSql("SELECT DATEDIFF_BIG(hh, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(hour, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG mi", {
  sql <- translateSql("SELECT DATEDIFF_BIG(mi, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG n", {
  sql <- translateSql("SELECT DATEDIFF_BIG(n, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(minute, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG ss", {
  sql <- translateSql("SELECT DATEDIFF_BIG(ss, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(second, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEDIFF_BIG mcs", {
  sql <- translateSql("SELECT DATEDIFF_BIG(mcs, drug_era_start_date, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEDIFF(microsecond, drug_era_start_date, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART dd", {
  sql <- translateSql("SELECT DATEPART(dd, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(day, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART m", {
  sql <- translateSql("SELECT DATEPART(m, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(month, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART mm", {
  sql <- translateSql("SELECT DATEPART(mm, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(month, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART yyyy", {
  sql <- translateSql("SELECT DATEPART(yyyy, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(year, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART yy", {
  sql <- translateSql("SELECT DATEPART(yy, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(year, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART qq", {
  sql <- translateSql("SELECT DATEPART(qq, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(quarter, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART q", {
  sql <- translateSql("SELECT DATEPART(q, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(quarter, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART wk", {
  sql <- translateSql("SELECT DATEPART(wk, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(week, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART ww", {
  sql <- translateSql("SELECT DATEPART(ww, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(week, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART hh", {
  sql <- translateSql("SELECT DATEPART(hh, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(hour, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART mi", {
  sql <- translateSql("SELECT DATEPART(mi, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART n", {
  sql <- translateSql("SELECT DATEPART(n, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(minute, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART ss", {
  sql <- translateSql("SELECT DATEPART(ss, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(second, drug_era_end_date) FROM drug_era;")
})

test_that("translateSQL sql server -> RedShift DATEPART mcs", {
  sql <- translateSql("SELECT DATEPART(mcs, drug_era_end_date) FROM drug_era;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT DATEPART(microsecond, drug_era_end_date) FROM drug_era;")
})


test_that("translateSQL sql server -> RedShift DATETIMEFROMPARTS", {
  sql <- translateSql("SELECT DATETIMEFROMPARTS(year,month,day,hour,minute,second,millisecond) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(second,'00FM')||'.'||TO_CHAR(millisecond,'000FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift EOMONTH", {
  sql <- translateSql("SELECT EOMONTH(date) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT LAST_DAY(date) FROM table")
})

test_that("translateSQL sql server -> RedShift VARIANCE", {
  sql <- translateSql("SELECT VAR(a) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT VARIANCE(a) FROM table")
})

test_that("translateSQL sql server -> RedShift SQUARE", {
  sql <- translateSql("SELECT SQUARE(a + b) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT ((a + b) * (a + b)) FROM table")
})

test_that("translateSQL sql server -> RedShift NEWID", {
  sql <- translateSql("SELECT NEWID()", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT MD5(RANDOM()::TEXT || GETDATE()::TEXT)")
})

test_that("translateSQL sql server -> RedShift BOOL TYPE", {
  sql <- translateSql("CREATE TABLE table ( col BIT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col BOOLEAN not null)")
})

test_that("translateSQL sql server -> RedShift MONEY TYPE", {
  sql <- translateSql("CREATE TABLE table ( col MONEY not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col DECIMAL(19, 4) not null)")
})

test_that("translateSQL sql server -> RedShift SMALLMONEY TYPE", {
  sql <- translateSql("CREATE TABLE table ( col SMALLMONEY not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col DECIMAL(10, 4) not null)")
})

test_that("translateSQL sql server -> RedShift TINYINT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col TINYINT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col SMALLINT not null)")
})

test_that("translateSQL sql server -> RedShift FLOAT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col FLOAT(@s) not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col FLOAT not null)")
})

test_that("translateSQL sql server -> RedShift DATETIME2 TYPE with precision specified", {
  sql <- translateSql("CREATE TABLE table ( col DATETIME2(@p) not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift DATETIME2 TYPE", {
  sql <- translateSql("CREATE TABLE table ( col DATETIME2 not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift DATETIME TYPE", {
  sql <- translateSql("CREATE TABLE table ( col DATETIME not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift SMALLDATETIME TYPE", {
  sql <- translateSql("CREATE TABLE table ( col SMALLDATETIME not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMP not null)")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSET TYPE with precision specified", {
  sql <- translateSql("CREATE TABLE table ( col DATETIMEOFFSET(@p) not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMPTZ not null)")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSET TYPE", {
  sql <- translateSql("CREATE TABLE table ( col DATETIMEOFFSET not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col TIMESTAMPTZ not null)")
})

test_that("translateSQL sql server -> RedShift TEXT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col TEXT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col VARCHAR(max) not null)")
})

test_that("translateSQL sql server -> RedShift NTEXT TYPE", {
  sql <- translateSql("CREATE TABLE table ( col NTEXT not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col VARCHAR(max) not null)")
})

test_that("translateSQL sql server -> RedShift UNIQUEIDENTIFIER TYPE", {
  sql <- translateSql("CREATE TABLE table ( col UNIQUEIDENTIFIER not null)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "CREATE TABLE table ( col CHAR(36) not null)")
})

test_that("translateSQL sql server -> RedShift STDEV POP", {
  sql <- translateSql("SELECT STDEVP(col) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT STDDEV_POP(col) FROM table")
})

test_that("translateSQL sql server -> RedShift VAR POP", {
  sql <- translateSql("SELECT VARP(col) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT VAR_POP(col) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIME2FROMPARTS", {
  sql <- translateSql("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds, 0, 0) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIME2FROMPARTS with fractions", {
  sql <- translateSql("SELECT DATETIME2FROMPARTS(year,month,day,hour,minute,seconds,fractions,precision) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0', precision) || 'FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSETFROMPARTS", {
  sql <- translateSql("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds, 0,h_offset,m_offset, 0) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table")
})

test_that("translateSQL sql server -> RedShift DATETIMEOFFSETFROMPARTS with fractions", {
  sql <- translateSql("SELECT DATETIMEOFFSETFROMPARTS(year,month,day,hour,minute,seconds,fractions,h_offset,m_offset,precision) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM')||':'||TO_CHAR(seconds,'00FM')||'.'||TO_CHAR(fractions,repeat('0',precision) || 'FM')||case when h_offset >= 0 then '+' else '-' end ||TO_CHAR(ABS(h_offset),'00FM')||':'||TO_CHAR(ABS(m_offset),'00FM') as TIMESTAMPTZ) FROM table")
})

test_that("translateSQL sql server -> RedShift GETUTCDATE", {
  sql <- translateSql("SELECT GETUTCDATE();", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translateSQL sql server -> RedShift SMALLDATETIMEFROMPARTS", {
  sql <- translateSql("SELECT SMALLDATETIMEFROMPARTS(year,month,day,hour,minute) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CAST(TO_CHAR(year,'0000FM')||'-'||TO_CHAR(month,'00FM')||'-'||TO_CHAR(day,'00FM')||' '||TO_CHAR(hour,'00FM')||':'||TO_CHAR(minute,'00FM') as TIMESTAMP) FROM table")
})

test_that("translateSQL sql server -> RedShift SYSUTCDATETIME", {
  sql <- translateSql("SELECT SYSUTCDATETIME();", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CURRENT_TIMESTAMP;")
})

test_that("translateSQL sql server -> RedShift ATN2", {
  sql <- translateSql("SELECT ATN2(a, b) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT ATAN2(a, b) FROM table")
})

test_that("translateSQL sql server -> RedShift TRUNCATION OF NUMBER", {
  sql <- translateSql("SELECT ROUND(expression,length,trunc) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT case when trunc = 0 then ROUND(CAST(expression AS FLOAT),length) else TRUNC(CAST(expression AS FLOAT),length) end FROM table")
})

test_that("translateSQL sql server -> RedShift CHARINDEX from position", {
  sql <- translateSql("SELECT CHARINDEX('test',column, 3) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT case when CHARINDEX('test', SUBSTRING(column, 3)) > 0 then (CHARINDEX('test', SUBSTRING(column, 3)) + 3 - 1) else 0 end FROM table")
})

test_that("translateSQL sql server -> RedShift QUOTENAME", {
  sql <- translateSql("SELECT QUOTENAME(a) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT QUOTE_IDENT(a) FROM table")
})

test_that("translateSQL sql server -> RedShift SPACE", {
  sql <- translateSql("SELECT SPACE(n) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT REPEAT(' ',n) FROM table")
})

test_that("translateSQL sql server -> RedShift STUFF", {
  sql <- translateSql("SELECT STUFF(expression, start, length, replace) FROM table", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT SUBSTRING(expression, 0, start)|| replace||SUBSTRING(expression, start + length) FROM table")
})

test_that("translateSQL sql server -> RedShift CONCAT", {
  sql <- translateSql(
    "SELECT CONCAT(p1,p2,p3,p4,p5,p6,p7) FROM table", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CONCAT(p1,CONCAT(p2,CONCAT(p3,CONCAT(p4,CONCAT(p5,CONCAT(p6,p7)))))) FROM table")
})

test_that("translateSQL sql server -> RedShift CONCAT", {
  sql <- translateSql(
    "SELECT CONCAT('Condition occurrence record observed during long_term_days on or prior to cohort index:  ', CAST((p1.covariate_id-101)/1000 AS VARCHAR), '-', CASE WHEN c1.concept_name IS NOT NULL THEN c1.concept_name ELSE 'Unknown invalid concept' END) FROM table", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CONCAT('Condition occurrence record observed during long_term_days on or prior to cohort index:  ',CONCAT(CAST((p1.covariate_id-101)/1000 AS VARCHAR),CONCAT('-',CASE WHEN c1.concept_name IS NOT NULL THEN c1.concept_name ELSE 'Unknown invalid concept' END))) FROM table")
})




test_that("translateSQL sql server -> RedShift CTAS TEMP WITH CTE person_id", {
  sql <- translateSql(
    "WITH a AS b SELECT person_id, col1, col2 INTO #table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS TEMP WITH CTE person_id at the end", {
  sql <- translateSql(
    "WITH a AS b SELECT col1, col2, person_id INTO #table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  col1, col2, person_id\nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS WITH CTE person_id", {
  sql <- translateSql(
    "WITH a AS b SELECT person_id, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(person_id)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS WITH CTE person_id with alias", {
  sql <- translateSql(
    "WITH a AS b SELECT person_id as dist, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n  person_id as dist, col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS WITH CTE person_id with alias at the end", {
  sql <- translateSql(
    "WITH a AS b SELECT col1, col2, person_id as dist INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, col2, person_id as dist \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS WITH CTE person_id with alias (no 'as')", {
  sql <- translateSql(
    "WITH a AS b SELECT col1, person_id dist, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, person_id dist, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS WITH CTE person_id with alias (no 'as') at the end", {
  sql <- translateSql(
    "WITH a AS b SELECT col1, col2, person_id dist INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nWITH\n a \nAS\n b \nSELECT\n col1, col2, person_id dist \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS TEMP person_id", {
  sql <- translateSql(
    "SELECT person_id, col1, col2 INTO #table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table \nDISTKEY(person_id)\nAS\nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS person_id", {
  sql <- translateSql(
    "SELECT person_id, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(person_id)\nAS\nSELECT\n  person_id , col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS person_id with alias", {
  sql <- translateSql(
    "SELECT person_id as dist, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n  person_id as dist, col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS person_id with alias at the end", {
  sql <- translateSql(
    "SELECT col1, col2, person_id as dist INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n col1, col2, person_id as dist \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS person_id with alias (no 'as')", {
  sql <- translateSql(
    "SELECT person_id dist, col1, col2 INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n  person_id dist, col1, col2 \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CTAS person_id with alias (no 'as') at the end", {
  sql <- translateSql(
    "SELECT col1, col2, person_id dist INTO table FROM person;", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table \nDISTKEY(dist)\nAS\nSELECT\n col1, col2, person_id dist \nFROM\n person;")
})

test_that("translateSQL sql server -> RedShift CREATE TABLE person_id", {
  sql <- translateSql(
    "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  [dbo].[drug_era]  ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nDISTKEY(person_id);")
})

test_that("translateSQL sql server -> PDW CREATE TABLE person_id", {
  sql <- translateSql(
    "CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);", 
    sourceDialect = "sql server", targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql, 
                             "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   [dbo].[drug_era]  ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> RedShift ISDATE", {
  sql <- translateSql(
    "SELECT * FROM table WHERE ISDATE(col) = 1", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "SELECT * FROM table WHERE REGEXP_INSTR(col, '^(\\\\d{4}[/\\-]?[01]\\\\d[/\\-]?[0123]\\\\d)([ T]([0-1][0-9]|[2][0-3]):([0-5][0-9])(:[0-5][0-9](.\\\\d+)?)?)?$') = 1")
})

test_that("translateSQL sql server -> RedShift ISNUMERIC", {
  sql <- translateSql(
    "SELECT * FROM table WHERE ISNUMERIC(col) = 1", 
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "SELECT * FROM table WHERE REGEXP_INSTR(col, '^[\\-\\+]?(\\\\d*\\\\.)?\\\\d+([Ee][\\-\\+]?\\\\d+)?$') = 1")
})

test_that("translateSQL sql server -> RedShift PATINDEX", {
  sql <- translateSql(
    "SELECT PATINDEX(pattern,expression) FROM table;",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "SELECT REGEXP_INSTR(expression, case when LEFT(pattern,1)<>'%' and RIGHT(pattern,1)='%' then '^' else '' end||TRIM('%' FROM REPLACE(pattern,'_','.'))||case when LEFT(pattern,1)='%' and RIGHT(pattern,1)<>'%' then '$' else '' end) FROM table;")
})

test_that("translateSQL sql server -> RedShift SELECT INTO temp table with CTE and default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(paste(
    "WITH cte(a1) AS (SELECT a1 FROM table_a)",
    "SELECT *",
    "INTO #table",
    "FROM cte;",
    sep = " "),
    sourceDialect = "sql server", targetDialect = "redshift")$sql
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

test_that("translateSQL sql server -> RedShift SELECT INTO permanent table with CTE and default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(paste(
    "WITH cte(a1) AS (SELECT a1 FROM table_a)",
    "SELECT *",
    "INTO table",
    "FROM cte;",
    sep = " "),
    sourceDialect = "sql server", targetDialect = "redshift")$sql
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

test_that("translateSQL sql server -> RedShift SELECT INTO temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(paste(
    "SELECT *",
    "INTO #table",
    "FROM another_table;",
    sep = " "),
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  #table  DISTSTYLE ALL",
    "AS",
    "SELECT",
    " * ",
    "FROM",
    " another_table;",
    sep = "\n"))
})

test_that("translateSQL sql server -> RedShift SELECT INTO permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(paste(
    "SELECT *",
    "INTO table",
    "FROM another_table;",
    sep = " "),
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, paste(
    "CREATE TABLE  table  DISTSTYLE ALL",
    "AS",
    "SELECT",
    " * ",
    "FROM",
    " another_table;",
    sep = "\n"))
})

test_that("translateSQL sql server -> RedShift SELECT value INTO temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(
    "SELECT a INTO #table;",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table DISTSTYLE ALL\nAS\nSELECT\n a ;")
})

test_that("translateSQL sql server -> RedShift SELECT value INTO permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(
    "SELECT a INTO table;",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table DISTSTYLE ALL\nAS\nSELECT\n a ;")
})

test_that("translateSQL sql server -> RedShift create temp table with default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(
    "CREATE TABLE #table (id int not null, col varchar(max));",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  #table  (id int not null, col varchar(max))\nDISTSTYLE ALL;")
})

test_that("translateSQL sql server -> RedShift create permanent table with default hashing (DISTSTYLE ALL)", {
  sql <- translateSql(
    "CREATE TABLE table (id int not null, col varchar(max));",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "CREATE TABLE  table  (id int not null, col varchar(max))\nDISTSTYLE ALL;")
})

test_that("translateSQL sql server -> RedShift CREATE TABLE IF NOT EXISTS with hashing", {
  sql <- translateSql(paste(    
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
    sourceDialect = "sql server", targetDialect = "redshift")$sql
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

test_that("translateSQL sql server -> RedShift DISTINCT + TOP", {
  sql <- translateSql(
    "SELECT DISTINCT TOP 100 * FROM table WHERE a = b;",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "SELECT TOP 100 DISTINCT * FROM table WHERE a = b;")
})

test_that("RedShift String literal within CTE should be explicitly casted to character type", {
  sql <- translateSql(
    "WITH expression AS(SELECT 'my literal' literal, col1, CAST('other literal' as VARCHAR(MAX)), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;",
    sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "WITH  expression  AS (SELECT CAST('my literal' as TEXT) literal, col1, CAST('other literal' as VARCHAR(MAX)), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;")
})

test_that("Postgres String literal within CTE should be explicitly casted to character type", {
  sql <- translateSql(
    "WITH expression AS(SELECT 'my literal', col1, CAST('other literal' as VARCHAR(MAX)), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;",
    sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal_ignore_spaces(sql, 
                             "WITH  expression  AS (SELECT CAST('my literal' as TEXT), col1, CAST('other literal' as TEXT), col2 FROM table WHERE a = b) SELECT * FROM expression ORDER BY 1, 2, 3, 4;")
})

test_that("RedShift XOR operator", {
  sql <- translateSql("select a ^ b from c where a = 1;", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select a # b from c where a = 1;")
})

test_that("translateSQL sql server -> redshift hint DISTKEY + SORTKEY on CTAS + CTE", {
  sql <- translateSql(
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(COMPOUND:start_date)\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT * INTO #my_table FROM cte;",
    targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(COMPOUND:start_date)\nCREATE TABLE #my_table\nDISTKEY(row_id)\nCOMPOUND SORTKEY(start_date)\nAS\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT\n * \nFROM\n cte;")
})

test_that("translateSQL sql server -> redshift hint SORTKEY on CTAS + CTE", {
  sql <- translateSql(
    "--HINT SORT_ON_KEY(COMPOUND:start_date)\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT * INTO #my_table FROM cte;",
    targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "--HINT SORT_ON_KEY(COMPOUND:start_date)\nCREATE TABLE #my_table\nCOMPOUND SORTKEY(start_date)\nAS\nWITH cte(row_id, start_date) AS (select * from basetable)\nSELECT\n * \nFROM\n cte;")
})

test_that("translateSQL sql server -> redshift hint DISTKEY + SORTKEY on CTAS", {
  sql <- translateSql(
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(:start_date, end_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(:start_date, end_date)\nCREATE TABLE #my_table\nDISTKEY(row_id)\nSORTKEY(start_date, end_date)\nAS\nSELECT\n*\nFROM\n other_table;")
})

test_that("translateSQL sql server -> redshift hint SORTKEY on CTAS", {
  sql <- translateSql(
    "--HINT SORT_ON_KEY(:start_date, end_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, 
                             "--HINT SORT_ON_KEY(:start_date, end_date)\nCREATE TABLE #my_table\nSORTKEY(start_date, end_date)\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> redshift hint DISTKEY + SORTKEY on CREATE TABLE", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date);",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date)\nDISTKEY(row_id)\nINTERLEAVED SORTKEY(start_date);")
})

test_that("translateSQL sql server -> redshift hint SORTKEY on CREATE TABLE", {
  sql <- translateSql("--HINT SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date);",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "--HINT SORT_ON_KEY(INTERLEAVED:start_date)\nCREATE TABLE cdm.my_table (row_id INT, start_date)\n\nINTERLEAVED SORTKEY(start_date);")
})

test_that("translateSQL sql server -> pdw hint DISTKEY + SORTKEY on CREATE TABLE", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nCREATE TABLE my_table (row_id INT, start_date DATE);",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\n\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE my_table (row_id INT, start_date DATE)\nWITH (DISTRIBUTION = HASH(row_id));")
})

test_that("translateSQL sql server -> pdw hint DISTKEY + SORTKEY on CTAS", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "pdw")$sql
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\n\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> redshift CONVERT to DATE", {
  sql <- translateSql("select CONVERT(DATE, start_date) from my_table;",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select CAST(start_date as DATE) from my_table;")
})

test_that("translateSQL sql server -> redshift CONVERT to TIMESTAMPTZ", {
  sql <- translateSql("select CONVERT(TIMESTAMPTZ, start_date) from my_table;",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select CONVERT(TIMESTAMP WITH TIME ZONE, start_date) from my_table;")
})

test_that("translateSQL sql server -> oracle add group by when case count", {
  sql <- translateSql("SELECT CASE COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1) END AS stat FROM my_table;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT  CASE  COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1)  END AS stat  FROM my_table  GROUP BY 1;")
})

test_that("translateSQL sql server -> oracle don't add group by when case count but already group by", {
  sql <- translateSql("SELECT CASE COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1) END AS stat FROM my_table GROUP BY y;",
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT  CASE  COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1)  END AS stat  FROM my_table GROUP BY y  ;")
})

test_that("translateSQL sql server -> Redshift partition window function sorted descending", {
  sql <- translateSql("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt desc) as count_value", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  DESC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translateSQL sql server -> Redshift partition window function sorted ascending", {
  sql <- translateSql("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt asc) as count_value", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  ASC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translateSQL sql server -> Redshift partition window function no sort specified", {
  sql <- translateSql("select sum(count(person_id)) over (PARTITION BY procedure_concept_id order by prc_cnt) as count_value", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (PARTITION BY procedure_concept_id  ORDER BY prc_cnt  ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translateSQL sql server -> Redshift partition window function with specified frame", {
  sql <- translateSql("select MAX(start_ordinal) OVER (PARTITION BY groupid ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select MAX(start_ordinal) OVER (PARTITION BY groupid ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal")
})

test_that("translateSQL sql server -> Redshift partition window function ROW_NUMBER no sort specified", {
  sql <- translateSql("select ROW_NUMBER() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select ROW_NUMBER() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function CUME_DIST no sort specified", {
  sql <- translateSql("select CUME_DIST() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select CUME_DIST() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function DENSE_RANK no sort specified", {
  sql <- translateSql("select DENSE_RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select DENSE_RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function PERCENT_RANK no sort specified", {
  sql <- translateSql("select PERCENT_RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select PERCENT_RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function RANK no sort specified", {
  sql <- translateSql("select RANK() over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select RANK() OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function LAG no sort specified", {
  sql <- translateSql("select LAG(mycol) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select LAG(mycol) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function LEAD no sort specified", {
  sql <- translateSql("select LEAD(mycol) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select LEAD(mycol) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift partition window function NTILE no sort specified", {
  sql <- translateSql("select NTILE(4) over (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select NTILE(4) OVER (PARTITION BY procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function sorted descending without partition by clause", {
  sql <- translateSql("select sum(count(person_id)) over (order by prc_cnt desc) as count_value", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (ORDER BY prc_cnt  DESC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translateSQL sql server -> Redshift window function sorted ascending without partition by clause", {
  sql <- translateSql("select sum(count(person_id)) over (order by prc_cnt asc) as count_value", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (ORDER BY prc_cnt  ASC ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translateSQL sql server -> Redshift window function no sort specified without partition by clause", {
  sql <- translateSql("select sum(count(person_id)) over (order by prc_cnt) as count_value", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select sum(count(person_id)) OVER (ORDER BY prc_cnt ROWS UNBOUNDED PRECEDING) as count_value")
})

test_that("translateSQL sql server -> Redshift window function ROW_NUMBER no sort specified without PARTITION BY clause", {
  sql <- translateSql("select ROW_NUMBER() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select ROW_NUMBER() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function CUME_DIST no sort specified without PARTITION BY clause", {
  sql <- translateSql("select CUME_DIST() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select CUME_DIST() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function DENSE_RANK no sort specified without PARTITION BY clause", {
  sql <- translateSql("select DENSE_RANK() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select DENSE_RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function PERCENT_RANK no sort specified without PARTITION BY clause", {
  sql <- translateSql("select PERCENT_RANK() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select PERCENT_RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function RANK no sort specified without PARTITION BY clause", {
  sql <- translateSql("select RANK() over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select RANK() OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function LAG no sort specified without PARTITION BY clause", {
  sql <- translateSql("select LAG(mycol) over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select LAG(mycol) OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function LEAD no sort specified without PARTITION BY clause", {
  sql <- translateSql("select LEAD(mycol) over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select LEAD(mycol) OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Redshift window function NTILE no sort specified without PARTITION BY clause", {
  sql <- translateSql("select NTILE(4) over (procedure_concept_id ORDER BY prc_cnt) as num", 
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "select NTILE(4) OVER (procedure_concept_id ORDER BY prc_cnt) as num")
})

test_that("translateSQL sql server -> Oracle union of two queries without FROM", {
  sql <- translateSql("SELECT 1,2 UNION SELECT 3,4;", 
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle union of three queries without FROM", {
  sql <- translateSql("SELECT 1,2 UNION SELECT 3,4 UNION SELECT 5,6;", 
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL UNION SELECT 5,6 FROM DUAL;")
})


test_that("translateSQL sql server -> Oracle insert plus union of three queries without FROM", {
  sql <- translateSql("INSERT INTO my_table (a, b) SELECT 1,2 UNION SELECT 3,4 UNION SELECT 5,6;", 
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "INSERT INTO my_table (a, b) SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL UNION SELECT 5,6 FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle union where only last query needs FROM DUAL", {
  sql <- translateSql("SELECT a,b FROM my_table UNION SELECT 5,6;", 
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT a,b FROM my_table UNION SELECT 5,6 FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle nested queries with EOLs", {
  sql <- translateSql("INSERT INTO test (a,b) SELECT a,b FROM (SELECT a,b FROM (SELECT a,b FROM my_table\n) nesti WHERE b = 2\n) nesto WHERE a = 1;", 
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "INSERT INTO test (a,b) SELECT a,b FROM (SELECT a,b FROM (SELECT a,b FROM my_table\n ) nesti WHERE b = 2\n ) nesto WHERE a = 1;")
})


test_that("translateSQL sql server -> Oracle nested queries with union", {
  sql <- translateSql("SELECT a,b FROM (SELECT a,b FROM x UNION ALL SELECT a,b FROM x) o;", 
                      targetDialect = "oracle")$sql
  expect_equal_ignore_spaces(sql, "SELECT a,b FROM (SELECT a,b FROM x UNION ALL SELECT a,b FROM x) o;")
})

