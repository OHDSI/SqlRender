library("testthat")

expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equal(string1, string2)
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
 expect_equal_ignore_spaces(sql, "TO_DATE('20000101', 'yyyymmdd');")
})

test_that("translateSQL sql server -> Oracle concatenate string operator", {
 sql <- translateSql("select distinct cast(cast(YEAR(observation_period_start_date) as varchar(4)) + '01' + '01' as date) as obs_year;",
 targetDialect = "oracle")$sql
 expect_equal_ignore_spaces(sql,
 "SELECT distinct cast(TO_CHAR(EXTRACT(YEAR FROM observation_period_start_date) ) || '01' || '01' as date) as obs_year FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle RIGHT functions", {
 sql <- translateSql("select RIGHT(x,4);",
 targetDialect = "oracle")$sql
 expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4) FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle complex query", {
 sql <- translateSql("select CAST(CAST(YEAR(x) AS VARCHAR(12)) + RIGHT('0'+MONTH(x),2) + '01' AS DATE);",
 targetDialect = "oracle")$sql
 expect_equal_ignore_spaces(sql,
 "SELECT CAST(TO_CHAR(EXTRACT(YEAR FROM x)  ) || SUBSTR('0' ||EXTRACT(MONTH FROM x),-2) || '01' AS DATE) FROM DUAL;")
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
 expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
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
 "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), ' YYYY- MM- DD') FROM table")
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

test_that("translateSQL sql server -> Impala RIGHT functions", {
 sql <- translateSql("SELECT RIGHT(x,4);",
 targetDialect = "impala")$sql
 expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4);")
})

test_that("translateSQL sql server -> Impala DELETE FROM", {
 sql <- translateSql("delete from ACHILLES_results;",
 targetDialect = "impala")$sql
 expect_equal_ignore_spaces(sql, "/* DELETE FROM ACHILLES_results; */")
})

test_that("translateSQL sql server -> Impala DELETE FROM WHERE", {
 sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
 targetDialect = "impala")$sql
 expect_equal_ignore_spaces(sql, "/* DELETE FROM ACHILLES_results where analysis_id IN (1, 2, 3); */")
})

test_that("translateSQL sql server -> Impala location reserved word", {
 sql <- translateSql("select count(1) from omop_cdm.location;",
 targetDialect = "impala")$sql
 expect_equal_ignore_spaces(sql, "select count(1) from omop_cdm.`location`;")
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

test_that("translateSQL sql server -> redshift TOP", {
 sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
 targetDialect = "redshift")$sql
 expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
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
 expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TEMP TABLE my_table DISTKEY(row_id)\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> redshift hint distribute_on_key", {
 sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE(row_id INT);",
 targetDialect = "redshift")$sql
 expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE (row_id INT) DISTKEY(row_id);")
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

test_that("translateSQL sql server -> redshift ISNUMERIC", {
  sql <- translateSql("SELECT CASE WHEN ISNUMERIC(a) THEN a ELSE b FROM c;",
                      targetDialect = "redshift")$sql
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN (a ~ '^([0-9]+\\.?[0-9]*|\\.[0-9]+)$') THEN a ELSE b FROM c;")
})

# For debugging: force reload of patterns:
# rJava::J("org.ohdsi.sql.SqlTranslate")$setReplacementPatterns("inst/csv/replacementPatterns.csv")
