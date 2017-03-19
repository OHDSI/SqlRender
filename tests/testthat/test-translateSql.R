library("testthat")

test_that("translateSQL sql server -> Oracle DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT   (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE))  FROM  drug_era ;")
})


test_that("translateSQL sql server -> Oracle DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT   (drug_era_end_date + interval '30' day)  FROM  drug_era ;")
})

test_that("translateSQL sql server -> Oracle USE", {
  sql <- translateSql("USE vocabulary;", sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "ALTER SESSION SET current_schema =  vocabulary;")
})

test_that("translateSQL sql server -> Oracle DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "BEGIN\n  EXECUTE IMMEDIATE 'TRUNCATE TABLE  cohort';\n  EXECUTE IMMEDIATE 'DROP TABLE  cohort';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;")
})


test_that("translateSQL sql server -> Oracle CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "CAST('20000101' AS DATE);")
})

test_that("translateSQL sql server -> Oracle CONVERT(AS DATE)", {
  sql <- translateSql("CONVERT(DATE, '20000101');",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "TO_DATE( '20000101', 'yyyymmdd');")
})

test_that("translateSQL sql server -> Oracle concatenate string operator", {
  sql <- translateSql("select distinct cast(cast(YEAR(observation_period_start_date) as varchar(4)) +  '01' + '01' as date) as obs_year;",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT  distinct cast(TO_CHAR(EXTRACT(YEAR FROM observation_period_start_date) ) || '01' || '01' as date) as obs_year FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle RIGHT functions", {
  sql <- translateSql("select RIGHT(x,4);",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT  SUBSTR(x,-4) FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle complex query", {
  sql <- translateSql("select CAST(CAST(YEAR(x) AS VARCHAR(12)) + RIGHT('0'+MONTH(x),2) + '01' AS DATE);",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT  CAST(TO_CHAR(EXTRACT(YEAR FROM x)  ) || SUBSTR('0' ||EXTRACT(MONTH FROM x),-2) || '01' AS DATE) FROM DUAL;")
})

test_that("translateSQL sql server -> Oracle '+' in quote", {
  sql <- translateSql("select '+';", sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT  '+' FROM DUAL;")
})

test_that("translateSQL sql server -> PostgreSQL USE", {
  sql <- translateSql("USE vocabulary;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "SET search_path TO  vocabulary;")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("'x' + b ( 'x' + b)",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "'x' || b ( 'x' || b)")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';b'", sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "a || ';b'")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';('", sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "a || ';('")
})


test_that("translateSQL sql server -> PostgreSQL add month", {
  sql <- translateSql("DATEADD(mm,1,date)",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "(date + 1*INTERVAL'1 month')")
})

test_that("translateSQL sql server -> Oracle multiple inserts in one statement", {
  sql <- translateSql("INSERT INTO my_table (key,value) VALUES (1,0),(2,0),(3,1)",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "INSERT ALL\nINTO   my_table   (key,value) VALUES (1,0)\n INTO  my_table  (key,value) VALUES (2,0)\n)\n INTO   my_table   (key,value) VALUES (3,1)\nSELECT * FROM dual")
})

test_that("translateSQL sql server -> RedShift VARCHAR(MAX)", {
  sql <- translateSql("VARCHAR(MAX)", sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "VARCHAR(MAX)")
})

test_that("translateSQL sql server -> Postgres WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql,
               "CREATE TABLE  d \nAS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO without FROM", {
  sql <- translateSql("SELECT c INTO d;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "CREATE TABLE  d AS\nSELECT\n c ;")
})


test_that("translateSQL sql server -> Postgres WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT   a  FROM  b ) SELECT   c  FROM  cte1 ;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "CREATE TABLE  d  \nAS\nWITH  cte1  AS  (SELECT   a  FROM  b )  SELECT\n   c \nFROM\n  cte1 ;")
})

test_that("translateSQL sql server -> Oracle WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "INSERT INTO  c (d int)  WITH  cte1  AS  (SELECT   a  FROM  b )  SELECT    e  FROM  cte1 ;")
})

test_that("translateSQL sql server -> PDW WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   d  WITH (DISTRIBUTION = REPLICATE)\nAS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> PDW WITH SELECT INTO temp table", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO #d FROM cte1;",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #d   WITH (LOCATION = USER_DB, DISTRIBUTION =  REPLICATE) AS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> PDW create temp table", {
  sql <- translateSql("CREATE TABLE #a (x int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a   (x int)\nWITH (LOCATION = USER_DB, DISTRIBUTION =  REPLICATE);")
})

test_that("translateSQL sql server -> PDW create temp table with person_id", {
  sql <- translateSql("CREATE TABLE #a (person_id int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a   ( person_id  int)\nWITH (LOCATION = USER_DB, DISTRIBUTION =  HASH(person_id));")
})

test_that("translateSQL sql server -> PDW create temp table with subject_id", {
  sql <- translateSql("CREATE TABLE #a (subject_id int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a   ( subject_id  int)\nWITH (LOCATION = USER_DB, DISTRIBUTION =  HASH(subject_id));")
})

test_that("translateSQL sql server -> PDW create temp table with analysis_id", {
  sql <- translateSql("CREATE TABLE #a (analysis_id int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #a   ( analysis_id  int)\nWITH (LOCATION = USER_DB, DISTRIBUTION =  HASH(analysis_id));")
})

test_that("translateSQL sql server -> PDW create permanent table", {
  sql <- translateSql("CREATE TABLE a (x int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   a  (x int)\nWITH (DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> PDW create permanent table with person_id", {
  sql <- translateSql("CREATE TABLE a (person_id int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   a  ( person_id  int)\nWITH (DISTRIBUTION = HASH(person_id));")
})

test_that("translateSQL sql server -> PDW create permanent table with subject_id", {
  sql <- translateSql("CREATE TABLE a (subject_id int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   a  ( subject_id  int)\nWITH (DISTRIBUTION = HASH(subject_id));")
})

test_that("translateSQL sql server -> PDW create permanent table with analysis_id", {
  sql <- translateSql("CREATE TABLE a (analysis_id int);",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   a  ( analysis_id  int)\nWITH (DISTRIBUTION = HASH(analysis_id));")
})

test_that("translateSQL sql server -> PDW select into permanent table", {
  sql <- translateSql("SELECT a INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   b  WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n a \nFROM\n c WHERE a = 1;")
})

test_that("translateSQL sql server -> PDW select into permanent table with person_id", {
  sql <- translateSql("SELECT a, person_id, b INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   b  WITH (DISTRIBUTION = HASH(person_id))\nAS\nSELECT\n a,  person_id,  b \nFROM\n c WHERE a = 1;")
})

test_that("translateSQL sql server -> PDW select into permanent table with analysis_id", {
  sql <- translateSql("SELECT a, analysis_id, b INTO b FROM c WHERE a = 1;",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   b  WITH (DISTRIBUTION = HASH(analysis_id))\nAS\nSELECT\n a,  analysis_id,  b \nFROM\n c WHERE a = 1;")
})

test_that("translateSQL sql server -> Postgres create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "CREATE TABLE IF NOT EXISTS  cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Redshift create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "redshift")$sql
  expect_equal(sql, "CREATE TABLE IF NOT EXISTS  cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Oracle create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "BEGIN\n  EXECUTE IMMEDIATE 'CREATE TABLE  cohort\n (cohort_definition_id INT)';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -955 THEN\n      RAISE;\n    END IF;\nEND;")
})

test_that("translateSQL sql server -> Oracle datefromparts", {
  sql <- translateSql("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), 'YYYY-MM-DD') FROM table")
})

test_that("translateSQL sql server -> redshift datefromparts", {
  sql <- translateSql("SELECT DATEFROMPARTS(year,month,day) FROM table",
                      targetDialect = "redshift")$sql
  expect_equal(sql,
               "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), ' YYYY- MM- DD') FROM table")
})


test_that("translateSQL sql server -> Oracle datetime to timestamp", {
  sql <- translateSql("CREATE TABLE x (a DATETIME)",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "CREATE TABLE x (a TIMESTAMP)")
})

test_that("translateSQL sql server -> Oracle select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT   column, ROW_NUMBER() OVER (ORDER BY DBMS_RANDOM.VALUE) AS rn  FROM  table ) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Postgres select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "postgresql")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Redshift select random row", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
                      targetDialect = "redshift")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RANDOM()) AS rn FROM table) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Oracle select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "oracle")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_CRYPTO.HASH(TO_CHAR(person_id ),2)) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Postgres select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "postgresql")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> Redshift select random row using hash", {
  sql <- translateSql("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
                      targetDialect = "redshift")$sql
  expect_equal(sql,
               "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS varchar))) tmp WHERE rn <= 1")
})

test_that("translateSQL sql server -> PDW cte with preceding 'with' in quotes", {
  sql <- translateSql("insert into x (a) values ('with'); with cte (a) as(select a from b) select a INTO #c from cte;",
                      targetDialect = "pdw")$sql
  expect_equal(sql,
               "insert into x (a) values ('with'); IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #c   WITH (LOCATION = USER_DB, DISTRIBUTION =  REPLICATE) AS\nWITH  cte (a)  AS (select a from b)  SELECT\n a \nFROM\n cte;")
})

test_that("translateSQL sql server throws error when invalid target is given", {
  expect_error(translateSql("iSELECT * FROM a;", targetDialect = "pwd")$sql)
})


test_that("translateSQL select into issue for pdw", {
  sql <- "SELECT @c1 INTO table FROM @c2 WHERE a = 1;"
  sql <- translateSql(sql, targetDialect = "pdw")$sql
  expect_equal(sql, "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   table  WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n @c1 \nFROM\n @c2 WHERE a = 1;")
})



test_that("translateSQL ## issue on oracle", {
  sql <- "SELECT a FROM c##blah.table;"
  sql <- translateSql(sql, targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT   a  FROM  c##blah.table ;")
})

# Impala tests

test_that("translateSQL sql server -> Impala USE", {
  sql <- translateSql("USE vocabulary;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "USE vocabulary;")
})

test_that("translateSQL sql server -> Impala CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "CASE TYPEOF('20000101' ) WHEN 'TIMESTAMP' THEN CAST('20000101'  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST('20000101'  AS STRING), 1, 4), SUBSTR(CAST('20000101'  AS STRING), 5, 2), SUBSTR(CAST('20000101'  AS STRING), 7, 2)), 'UTC') END;")
})

test_that("translateSQL sql server -> Impala DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "SELECT DATEDIFF(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, CASE TYPEOF(drug_era_start_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_start_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_start_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_start_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_start_date  AS STRING), 7, 2)), 'UTC') END) FROM drug_era;")
})

test_that("translateSQL sql server -> Impala DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "SELECT DATE_ADD(CASE TYPEOF(drug_era_end_date ) WHEN 'TIMESTAMP' THEN CAST(drug_era_end_date  AS TIMESTAMP) ELSE TO_UTC_TIMESTAMP(CONCAT_WS('-', SUBSTR(CAST(drug_era_end_date  AS STRING), 1, 4), SUBSTR(CAST(drug_era_end_date  AS STRING), 5, 2), SUBSTR(CAST(drug_era_end_date  AS STRING), 7, 2)), 'UTC') END, 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Impala WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Impala WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql,
               "CREATE TABLE  d \nAS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> Impala WITH SELECT INTO without FROM", {
  sql <- translateSql("SELECT c INTO d;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "CREATE TABLE  d AS\nSELECT\n c ;")
})

test_that("translateSQL sql server -> Impala create table if not exists", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "CREATE TABLE IF NOT EXISTS  cohort\n (cohort_definition_id INT);")
})

test_that("translateSQL sql server -> Impala DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql,
               "DROP TABLE IF EXISTS  cohort;")
})

test_that("translateSQL sql server -> Impala RIGHT functions", {
  sql <- translateSql("SELECT RIGHT(x,4);",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "SELECT SUBSTR(x,-4);")
})

test_that("translateSQL sql server -> Impala DELETE FROM", {
  sql <- translateSql("delete from ACHILLES_results;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "/* DELETE FROM  ACHILLES_results; */")
})

test_that("translateSQL sql server -> Impala DELETE FROM WHERE", {
  sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "/* DELETE FROM  ACHILLES_results where analysis_id IN (1, 2, 3); */")
})

test_that("translateSQL sql server -> Impala location reserved word", {
  sql <- translateSql("select count(1) from omop_cdm.location;",
                      sourceDialect = "sql server",
                      targetDialect = "impala")$sql
  expect_equal(sql, "select count(1) from omop_cdm.`location`;")
})


# Netezza tests

test_that("translateSQL sql server -> Netezza CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "TO_DATE('20000101' , 'yyyymmdd');")
})

test_that("translateSQL sql server -> Netezza DATEDIFF", {
  sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "SELECT (CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translateSQL sql server -> Netezza DATEADD", {
  sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
})

test_that("translateSQL sql server -> Netezza WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Netezza WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql,
               "CREATE TABLE  d \nAS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> Netezza DROP TABLE IF EXISTS", {
  sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql,
               "DROP TABLE  cohort IF EXISTS;")
})

test_that("translateSQL sql server -> Netezza RIGHT functions", {
  sql <- translateSql("SELECT RIGHT(x,4);",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "SELECT STRRIGHT(x,4);")
})

test_that("translateSQL sql server -> Netezza DELETE FROM WHERE", {
  sql <- translateSql("delete from ACHILLES_results where analysis_id IN (1, 2, 3);",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "delete from ACHILLES_results where analysis_id IN (1, 2, 3);")
})

test_that("translateSQL sql server -> Netezza CAST AS VARCHAR", {
  sql <- translateSql("CAST(person_id AS VARCHAR);",
                      sourceDialect = "sql server",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "CAST(person_id  AS VARCHAR(1000));")
})

test_that("translateSQL sql server -> PostgreSql TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "SELECT  * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> Oracle TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "oracle")$sql
  expect_equal(sql, "SELECT   *   FROM   my_table    WHERE   a = b   AND  ROWNUM <= 10; ")
})

test_that("translateSQL sql server -> redshift TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "redshift")$sql
  expect_equal(sql, "SELECT  * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> impala TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "impala")$sql
  expect_equal(sql, "SELECT  * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> redshift TOP", {
  sql <- translateSql("SELECT TOP 10 * FROM my_table WHERE a = b;",
                      targetDialect = "netezza")$sql
  expect_equal(sql, "SELECT  * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translateSQL sql server -> postgres date to varchar", {
  sql <- translateSql("SELECT CONVERT(VARCHAR,start_date,112) FROM table;",
                      targetDialect = "postgresql")$sql
  expect_equal(sql, "SELECT TO_CHAR(start_date, 'YYYYMMDD') FROM table;")
})


test_that("translateSQL sql server -> pdw hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "pdw")$sql
  expect_equal(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE  #my_table   WITH (LOCATION = USER_DB, DISTRIBUTION =  HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> pdw hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE(row_id INT);",
                      targetDialect = "pdw")$sql
  expect_equal(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE   (row_id INT)\nWITH (DISTRIBUTION = HASH(row_id));")
})


test_that("translateSQL sql server -> redshift hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
                      targetDialect = "redshift")$sql
  expect_equal(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TEMP TABLE my_table  DISTKEY(row_id)\nAS\nSELECT\n * \nFROM\n other_table;")
})

test_that("translateSQL sql server -> redshift hint distribute_on_key", {
  sql <- translateSql("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE(row_id INT);",
                      targetDialect = "redshift")$sql
  expect_equal(sql, "--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE  (row_id INT) DISTKEY(row_id);")
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
  expect_equal(sql, "SELECT   CONCAT(a, CONCAT(\" , \", CONCAT( c, CONCAT( d, e))))  FROM  x ;")
})

