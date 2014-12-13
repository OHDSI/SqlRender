library("testthat")

test_that("translateSQL sql server -> Oracle DATEDIFF", {
			sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "SELECT (drug_era_end_date - drug_era_start_date) FROM drug_era;")
		})


test_that("translateSQL sql server -> Oracle DATEADD", {
			sql <- translateSql("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
		})

test_that("translateSQL sql server -> Oracle USE", {
			sql <- translateSql("USE vocabulary;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "ALTER SESSION SET current_schema =  vocabulary;")
		})

test_that("translateSQL sql server -> Oracle DROP TABLE IF EXISTS", {
			sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "BEGIN\n  EXECUTE IMMEDIATE 'TRUNCATE TABLE  cohort';\n  EXECUTE IMMEDIATE 'DROP TABLE  cohort';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;")
		})


test_that("translateSQL sql server -> Oracle CAST(AS DATE)", {
  sql <- translateSql("CAST('20000101' AS DATE);",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "TO_DATE('20000101' , 'yyyymmdd');")
})

test_that("translateSQL sql server -> Oracle concatenate string operator", {
  sql <- translateSql("select distinct cast(cast(YEAR(observation_period_start_date) as varchar(4)) +  '01' + '01' as date) as obs_year;",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "select distinct TO_DATE(TO_CHAR(EXTRACT(YEAR FROM observation_period_start_date) ) || '01' || '01' , 'yyyymmdd') as obs_year;")
})

test_that("translateSQL sql server -> Oracle RIGHT functions", {
  sql <- translateSql("select RIGHT(x,4);",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "select SUBSTR(x,-4);")
})

test_that("translateSQL sql server -> Oracle complex query", {
  sql <- translateSql("select CAST(CAST(YEAR(x) AS VARCHAR(12)) + RIGHT('0'+MONTH(x),2) + '01' AS DATE);",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "select TO_DATE(TO_CHAR(EXTRACT(YEAR FROM x) ) || SUBSTR('0' ||EXTRACT(MONTH FROM x),-2) || '01' , 'yyyymmdd');")
})

test_that("translateSQL sql server -> Oracle '+' in quote", {
  sql <- translateSql("select '+';",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "select '+';")
})

test_that("translateSQL sql server -> PostgreSQL USE", {
  sql <- translateSql("USE vocabulary;",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "SET search_path TO  vocabulary;")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("'x' + b ( 'x' + b)",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "'x' || b ( 'x' || b)")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';b'",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "a || ';b'")
})

test_that("translateSQL sql server -> PostgreSQL string concat", {
  sql <- translateSql("a + ';('",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "a || ';('")
})


test_that("translateSQL sql server -> PostgreSQL add month", {
  sql <- translateSql("DATEADD(mm,1,date)",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "CAST((date + 1*INTERVAL'1 month') AS DATE)")
})

test_that("translateSQL sql server -> Oracle multiple inserts in one statement", {
  sql <- translateSql("INSERT INTO my_table (key,value) VALUES (1,0),(2,0),(3,1)",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "INSERT ALL\nINTO   my_table   (key,value) VALUES (1,0)\n INTO  my_table  (key,value) VALUES (2,0)\n)\n INTO   my_table   (key,value) VALUES (3,1)\nSELECT * FROM dual")
})

test_that("translateSQL sql server -> RedShift VARCHAR(MAX)", {
  sql <- translateSql("VARCHAR(MAX)",sourceDialect = "sql server", targetDialect = "redshift")$sql
  expect_equal(sql, "VARCHAR(MAX)")
})

test_that("translateSQL sql server -> Postgres WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Postgres WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "CREATE TABLE  d \nAS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})


test_that("translateSQL sql server -> Postgres WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",sourceDialect = "sql server", targetDialect = "postgresql")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translateSQL sql server -> Oracle WITH SELECT INTO", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "CREATE TABLE  d \nAS\nWITH  cte1  AS  (SELECT a FROM b)  SELECT\n c \nFROM\n cte1;")
})

test_that("translateSQL sql server -> Oracle WITH INSERT INTO SELECT", {
  sql <- translateSql("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",sourceDialect = "sql server", targetDialect = "oracle")$sql
  expect_equal(sql, "INSERT INTO  c (d int)  WITH  cte1  AS  (SELECT a FROM b)  SELECT  e FROM cte1;")
})

test_that("translateSQL sql server -> PDW create temp table", {
  sql <- translateSql("CREATE TABLE #a (x int);",sourceDialect = "sql server", targetDialect = "pdw")$sql
  expect_equal(sql, "CREATE TABLE #a  (x int)\nWITH ( LOCATION = USER_DB, DISTRIBUTION = REPLICATE);")
})

test_that("translateSQL sql server -> PDW create temp table with person_id", {
  sql <- translateSql("CREATE TABLE #a (person_id int);",sourceDialect = "sql server", targetDialect = "pdw")$sql
  expect_equal(sql, "CREATE TABLE #a  ( person_id  int)\nWITH ( LOCATION = USER_DB, DISTRIBUTION = HASH(person_id));")
})

