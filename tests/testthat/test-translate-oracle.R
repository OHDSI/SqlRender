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

test_that("translate sql server -> Oracle DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CEIL(CAST(drug_era_end_date AS DATE) - CAST(drug_era_start_date AS DATE)) FROM drug_era;"
  )
  
  sql <- translate("SELECT DATEDIFF(second,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT EXTRACT(SECOND FROM (drug_era_end_date - drug_era_start_date)) FROM drug_era;"
  )
  
  sql <- translate("SELECT DATEDIFF(minute,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT EXTRACT(MINUTE FROM (drug_era_end_date - drug_era_start_date)) FROM drug_era;"
  )
  
  sql <- translate("SELECT DATEDIFF(hour,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT EXTRACT(HOUR FROM (drug_era_end_date - drug_era_start_date)) FROM drug_era;"
  )
})

test_that("translate sql server -> Oracle DATEDIFF year", {
  sql <- translate("SELECT DATEDIFF(YEAR,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT (EXTRACT(YEAR FROM CAST(drug_era_end_date AS DATE)) - EXTRACT(YEAR FROM CAST(drug_era_start_date AS DATE))) FROM drug_era;"
  )
})

test_that("translate sql server -> Oracle DATEDIFF(MONTH)", {
  sql <- translate("SELECT DATEDIFF(month,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(sql, "SELECT MONTHS_BETWEEN(CAST(drug_era_end_date AS DATE), CAST(drug_era_start_date AS DATE)) FROM drug_era;")
})

test_that("translate sql server -> Oracle DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT (drug_era_end_date + NUMTODSINTERVAL(30, 'day')) FROM drug_era;"
  )
})

test_that("translate sql server -> Oracle functional index", {
  sql <- translate("CREATE INDEX name1 ON someTable (firstColumn,secondColumn) WHERE someCondition;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE INDEX name1 ON someTable (CASE WHEN someCondition THEN firstColumn END, CASE WHEN someCondition THEN secondColumn END);"
  )
})

test_that("translate sql server -> Oracle USE", {
  sql <- translate("USE vocabulary;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "ALTER SESSION SET current_schema = vocabulary;")
})

test_that("translate sql server -> Oracle DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "BEGIN\n EXECUTE IMMEDIATE 'TRUNCATE TABLE cohort';\n EXECUTE IMMEDIATE 'DROP TABLE cohort';\nEXCEPTION\n WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;"
  )
})

test_that("translate sql server -> Oracle CAST(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101', 'YYYYMMDD');")
})

test_that("translate sql server -> Oracle CAST(AS DATE) when not a character string", {
  sql <- translate("CAST(some_date_time AS DATE);", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "CAST(some_date_time AS DATE);")
})

test_that("translate sql server -> Oracle CONVERT(AS DATE)", {
  sql <- translate("CONVERT(DATE, '20000101');", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "TO_DATE('20000101', 'YYYYMMDD');")
})

test_that("translate sql server -> Oracle concatenate string operator", {
  sql <- translate("select distinct CONVERT(DATE, cast(YEAR(observation_period_start_date) as varchar(4)) + '01' + '01') as obs_year from observation_period;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT distinct TO_DATE(cast(EXTRACT(YEAR FROM observation_period_start_date) as varchar(4)) || '01' || '01', 'YYYYMMDD') as obs_year  FROM observation_period ;"
  )
})

test_that("translate sql server -> Oracle RIGHT functions", {
  sql <- translate("select RIGHT(x,4);", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT SUBSTR(x,-4) FROM DUAL;")
})

test_that("translate sql server -> Oracle complex query", {
  sql <- translate("select CONVERT(DATE,CAST(YEAR(DATEFROMPARTS(2000,1,1)) AS VARCHAR(12)) + RIGHT('0'+MONTH(DATEFROMPARTS(2000,1,1)),2) + '01') as X;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT TO_DATE(CAST(EXTRACT(YEAR FROM TO_DATE(TO_CHAR(2000,'0000')||'-'||TO_CHAR(1,'00')||'-'||TO_CHAR(1,'00'), 'YYYY-MM-DD'))  AS varchar(12)) || SUBSTR('0' ||EXTRACT(MONTH FROM TO_DATE(TO_CHAR(2000,'0000')||'-'||TO_CHAR(1,'00')||'-'||TO_CHAR(1,'00'), 'YYYY-MM-DD')),-2) || '01', 'YYYYMMDD') as X FROM DUAL;"
  )
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
  sql <- translate("SELECT a, b FROM a AS a1 JOIN b AS b1 ON a = b WHERE c = 1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(sql, "SELECT a, b FROM a a1 JOIN b b1 ON a = b WHERE c = 1;")
  sql <- translate("SELECT a, b FROM a as a1 INNER JOIN b AS b1 ON a = b LEFT JOIN c AS c1 ON b = c WHERE c IN (1,2,4);",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT a, b FROM a a1 INNER JOIN b b1 ON a = b LEFT JOIN c c1 ON b = c WHERE c IN (1,2,4);"
  )
  sql <- translate("SELECT a, b, d FROM a AS a1, b AS b1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a, b, d FROM a a1, b b1;")
  sql <- translate("SELECT a, b, d FROM a AS a1, b AS b1, c AS c1 WHERE c = 1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(sql, "SELECT a, b, d FROM a a1, b b1, c c1 WHERE c = 1;")
  sql <- translate("SELECT a, b, d FROM a AS a1,(SELECT c AS c1 FROM b AS b1) AS d1 WHERE c = 1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT a, b, d FROM a a1,(SELECT c AS c1 FROM b b1) d1 WHERE c = 1;"
  )
})

test_that("translate sql server -> Oracle multiple inserts in one statement", {
  sql <- translate("INSERT INTO my_table (key,value) VALUES (1,0),(2,0),(3,1)",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT ALL\nINTO my_table   (key,value) VALUES (1,0)\n INTO my_table  (key,value) VALUES (2,0)\n)\n INTO my_table   (key,value) VALUES (3,1)\nSELECT * FROM dual"
  )
})


test_that("translate sql server -> Oracle WITH SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "WITH cte1 AS (SELECT a FROM b) SELECT c FROM cte1;")
})

test_that("translate sql server -> Oracle WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> Oracle WITH INSERT INTO SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT INTO c (d int)  WITH cte1 AS (SELECT a FROM b)  SELECT e FROM cte1;"
  )
})

test_that("translate sql server -> Oracle create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "BEGIN\n EXECUTE IMMEDIATE 'CREATE TABLE cohort\n (cohort_definition_id INT)';\nEXCEPTION\n WHEN OTHERS THEN\n IF SQLCODE != -955 THEN\n RAISE;\n END IF;\nEND;"
  )
})

test_that("translate sql server -> Oracle datefromparts", {
  sql <- translate("SELECT DATEFROMPARTS(year,month,day) FROM table", targetDialect = "oracle")
  expect_equal_ignore_spaces(
    sql,
    "SELECT TO_DATE(TO_CHAR(year,'0000')||'-'||TO_CHAR(month,'00')||'-'||TO_CHAR(day,'00'), 'YYYY-MM-DD') FROM table"
  )
})

test_that("translate sql server -> Oracle datetime to timestamp", {
  sql <- translate("CREATE TABLE x (a DATETIME)", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP)")
})

test_that("translate sql server -> Oracle select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_RANDOM.VALUE) AS rn FROM table ) tmp WHERE rn <= 1"
  )
})


test_that("translate sql server -> Oracle select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY DBMS_CRYPTO.HASH(TO_CHAR(person_id ),2)) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> Oracle SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROW_NUMBER() OVER TO_NUMBER(val, RPAD('X', LENGTH(val), 'X')) rn WHERE rn <= 1"
  )
})

test_that("translate ## issue on oracle", {
  sql <- "SELECT a FROM c##blah.table;"
  sql <- translate(sql, targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a FROM c##blah.table;")
})

test_that("translate sql server -> Oracle TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT  * FROM my_table   WHERE a = b  FETCH FIRST 10 ROWS ONLY;")
})

test_that("translate sql server -> Oracle TOP subquery", {
  sql <- translate("SELECT name FROM (SELECT TOP 1 name FROM my_table WHERE a = b);",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT name FROM (SELECT  name FROM my_table   WHERE a = b  FETCH FIRST 1 ROWS ONLY) ;"
  )
})

test_that("translate sql server -> Oracle DISTINCT TOP", {
  sql <- translate("SELECT DISTINCT TOP 10 a FROM my_table WHERE a = b;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT DISTINCT a FROM my_table   WHERE a = b  FETCH FIRST 10 ROWS ONLY;")
})


test_that("translate sql server -> oracle concat", {
  sql <- translate("SELECT CONCAT(a,\" , \",c,d,e) FROM x;", targetDialect = "oracle")
  expect_equal_ignore_spaces(
    sql,
    "SELECT CONCAT(a, CONCAT(\" , \", CONCAT( c, CONCAT( d, e)))) FROM x;"
  )
})

test_that("translate sql server -> oracle natural log", {
  sql <- translate("SELECT LOG(number) FROM table", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT LOG(2.718281828459,number) FROM table")
})

test_that("translate sql server -> oracle log base 10", {
  sql <- translate("SELECT LOG10(number) FROM table;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT LOG(10,number) FROM table;")
})

test_that("translate sql server -> oracle log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT LOG( base,number) FROM table")
})


test_that("translate sql server -> oracle union 1", {
  sql <- translate("SELECT * FROM table1 WHERE a = 1 UNION SELECT * FROM table2 WHERE a = 1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT * FROM table1 WHERE a = 1 UNION SELECT * FROM table2 WHERE a = 1;"
  )
})

test_that("translate sql server -> oracle union 2", {
  sql <- translate("SELECT * FROM table1 UNION SELECT * FROM table2 WHERE a = 1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(sql, "SELECT * FROM table1 UNION SELECT * FROM table2 WHERE a = 1;")
})

test_that("translate sql server -> oracle from dual", {
  sql <- translate("SELECT 1 AS id;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT 1 AS id FROM DUAL;")
})

test_that("translate sql server -> oracle from dual", {
  sql <- translate("SELECT (SELECT id FROM a WHERE b=2) AS id;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT (SELECT id FROM a WHERE b=2) AS id FROM DUAL;")
})

test_that("translate sql server -> oracle ISNUMERIC", {
  sql <- translate("SELECT CASE WHEN ISNUMERIC(a) = 1 THEN a ELSE b FROM c;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CASE WHEN CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN 1 ELSE 0 END = 1 THEN a ELSE b  FROM c ;"
  )
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "oracle")
  expect_equal_ignore_spaces(
    sql,
    "SELECT a FROM table WHERE CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN 1 ELSE 0 END = 1"
  )
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "oracle")
  expect_equal_ignore_spaces(
    sql,
    "SELECT a FROM table WHERE CASE WHEN (LENGTH(TRIM(TRANSLATE(a, ' +-.0123456789',' '))) IS NULL) THEN 1 ELSE 0 END = 0"
  )
})

test_that("translate sql server -> oracle add group by when case count", {
  sql <- translate("SELECT CASE COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1) END AS stat FROM my_table;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT  CASE  COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1)  END AS stat  FROM my_table  GROUP BY 1;"
  )
})

test_that("translate sql server -> oracle don't add group by when case count but already group by", {
  sql <- translate("SELECT CASE COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1) END AS stat FROM my_table GROUP BY y;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT  CASE  COUNT(*) = 1 THEN 0 ELSE SUM(x)/(COUNT(*)-1)  END AS stat  FROM my_table GROUP BY y  ;"
  )
})


test_that("translate sql server -> Oracle union of two queries without FROM", {
  sql <- translate("SELECT 1,2 UNION SELECT 3,4;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL;")
})

test_that("translate sql server -> Oracle union of three queries without FROM", {
  sql <- translate("SELECT 1,2 UNION SELECT 3,4 UNION SELECT 5,6;", targetDialect = "oracle")
  expect_equal_ignore_spaces(
    sql,
    "SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL UNION SELECT 5,6 FROM DUAL;"
  )
})


test_that("translate sql server -> Oracle insert plus union of three queries without FROM", {
  sql <- translate("INSERT INTO my_table (a, b) SELECT 1,2 UNION SELECT 3,4 UNION SELECT 5,6;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT INTO my_table (a, b) SELECT 1,2 FROM DUAL UNION SELECT 3,4 FROM DUAL UNION SELECT 5,6 FROM DUAL;"
  )
})

test_that("translate sql server -> Oracle union where only last query needs FROM DUAL", {
  sql <- translate("SELECT a,b FROM my_table UNION SELECT 5,6;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT a,b FROM my_table UNION SELECT 5,6 FROM DUAL;")
})

test_that("translate sql server -> Oracle nested queries with EOLs", {
  sql <- translate("INSERT INTO test (a,b) SELECT a,b FROM (SELECT a,b FROM (SELECT a,b FROM my_table\n) nesti WHERE b = 2\n) nesto WHERE a = 1;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "INSERT INTO test (a,b) SELECT a,b FROM (SELECT a,b FROM (SELECT a,b FROM my_table\n ) nesti WHERE b = 2\n ) nesto WHERE a = 1;"
  )
})

test_that("translate sql server -> Oracle nested queries with union", {
  sql <- translate("SELECT a,b FROM (SELECT a,b FROM x UNION ALL SELECT a,b FROM x) o;",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT a,b FROM (SELECT a,b FROM x UNION ALL SELECT a,b FROM x) o;"
  )
})

test_that("translate sql server -> Oracle BIGINT in conditional create table", {
  sql <- translate("IF OBJECT_ID('test', 'U') IS NULL CREATE TABLE test (x BIGINT);",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "BEGIN\n  EXECUTE IMMEDIATE 'CREATE TABLE test  (x NUMBER(19))';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -955 THEN\n      RAISE;\n    END IF;\nEND;"
  )
})

test_that("translate sql server -> Oracle NOT NULL and DEFAULT in conditional create table", {
  sql <- translate("IF OBJECT_ID('test_b', 'U') IS NULL CREATE TABLE test_b (x INT NOT NULL DEFAULT 0);",
    targetDialect = "oracle"
  )
  expect_equal_ignore_spaces(
    sql,
    "BEGIN\n  EXECUTE IMMEDIATE 'CREATE TABLE test_b  (x INT DEFAULT 0 NOT NULL)';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -955 THEN\n      RAISE;\n    END IF;\nEND;"
  )
})

test_that("translate sql server -> Oracle analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "-- ANALYZE should not be used to collect optimizer statistics")
})

test_that("translate sql server -> Oracle DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a TIMESTAMP, b TIMESTAMP);")
})


test_that("translateSingleStatement sql server -> oracle with trailing ;", {
  sql <- translateSingleStatement("SELECT * FROM my_table;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table ")
})

test_that("translateSingleStatement sql server -> oracle with trailing ; but BEGIN END", {
  sql <- translateSingleStatement("BEGIN\nSELECT * INTO a FROM b;\nEND;", targetDialect = "oracle")
  expect_equal_ignore_spaces(
    sql,
    "BEGIN \n CREATE TABLE a AS \n SELECT \n * \n FROM \n b ; \n END;"
  )
})

test_that("translateSingleStatement sql server -> oracle throw error if > 1 statement", {
  expect_error(sql <- translateSingleStatement("TRUNCATE my_table; DROP TABLE my_table;",
    targetDialect = "oracle"
  ))
})

test_that("translate sql server -> oracle DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "BEGIN\n EXECUTE IMMEDIATE 'TRUNCATE TABLE test';\n EXECUTE IMMEDIATE 'DROP TABLE test';\nEXCEPTION\n WHEN OTHERS THEN\n IF SQLCODE != -942 THEN\n RAISE;\n END IF;\nEND;")
})

test_that("translate sql server -> oracle SELECT *,", {
  sql <- translate("SELECT *, 1 AS x FROM my_table;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT my_table .*, 1 AS x  FROM my_table ;")
})

test_that("translate sql server -> oracle SELECT *,", {
  sql <- translate("SELECT *, 1 AS x FROM (SELECT a FROM b) q01;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT q01 .*, 1 AS x  FROM (SELECT a FROM b ) q01;")
})

test_that("translate sql server -> oracle SELECT TOP *,", {
  sql <- translate("SELECT TOP 10 *, 1 AS x FROM my_table;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT my_table  .*, 1 AS x  FROM my_table   FETCH FIRST 10 ROWS ONLY;")
})

test_that("translate sql server -> oracle SELECT *, FROM", {
  sql <- translate("SELECT *, 1 AS x FROM my_table WHERE a = b;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT my_table   .*, 1 AS x  FROM my_table    WHERE a = b ;")
})

test_that("translate sql server -> oracle SELECT *, FROM ORDER BY", {
  sql <- translate("SELECT *, 1 AS x FROM my_table WHERE a = b ORDER BY a;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT my_table   .*, 1 AS x  FROM my_table    WHERE a = b ORDER BY a ;")
})

test_that("translate sql server -> oracle nested SELECT *, FROM", {
  sql <- translate("(SELECT *, 1 AS x FROM my_table)", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "(SELECT my_table .*, 1 AS x  FROM my_table )")
})

test_that("translate sql server -> oracle IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table ;")
})

test_that("translate: warning when using oracleTempSchema", {
  clearWarningBlock()
  expect_warning(translate("SELECT * FROM #my_table", targetDialect = "oracle", oracleTempSchema = "scratch"))
})

test_that("translateSingleStatement: warning when using oracleTempSchema", {
  clearWarningBlock()
  expect_warning(translateSingleStatement("SELECT * FROM #my_table", targetDialect = "oracle", oracleTempSchema = "scratch"))
})

test_that("translate sql server -> oracle drvd()", {
  sql <- translate("SELECT
      TRY_CAST(name AS VARCHAR(MAX)) AS name,
      TRY_CAST(speed AS FLOAT) AS speed
    FROM (  VALUES ('A', 1.0), ('B', 2.0)) AS drvd(name, speed);", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT CAST(name AS VARCHAR2(1024)) AS name,\n      CAST(speed AS FLOAT) AS speed\n    FROM (SELECT NULL AS name, NULL AS speed    FROM DUAL WHERE (0 = 1)    UNION ALL SELECT 'A', 1.0   FROM DUAL   UNION ALL SELECT 'B', 2.0  FROM DUAL )   values_table ;")
})

test_that("translate sql server -> oracle temp table field ref", {
  sql <- translate("SELECT #tmp.name FROM #tmp;", targetDialect = "oracle", tempEmulationSchema = "ts")
  expect_equal_ignore_spaces(sql, sprintf("SELECT %stmp.name FROM ts.%stmp;", getTempTablePrefix(), getTempTablePrefix()))
})

test_that("translate sql server -> oracle temp dplyr ... pattern", {
  sql <- translate("SELECT * FROM table...1;", targetDialect = "oracle")
  expect_equal_ignore_spaces(sql, "SELECT * FROM tablexxx1;")
})
