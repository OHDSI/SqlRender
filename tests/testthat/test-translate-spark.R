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

test_that("translate sql server -> spark round", {
  sql <- translate("SELECT round(3.14, 1)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT ROUND(CAST(3.14 AS float),1)"
  )
})


test_that("translate sql server -> spark select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY MD5(CAST(person_id AS STRING))) tmp WHERE rn <= 1"
  )
})


test_that("translate sql server -> spark SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1")
})


test_that("translate sql server -> spark convert date", {
  sql <- translate("SELECT convert(date, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT TO_DATE('2019-01-01', 'yyyy-MM-dd')")
})


test_that("translate sql server -> spark dateadd", {
  sql <- translate("SELECT dateadd(second, 1, '2019-01-01 00:00:00')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01 00:00:00' + INTERVAL 1 second)")

  sql <- translate("SELECT dateadd(minute, 1, '2019-01-01 00:00:00')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01 00:00:00' + INTERVAL 1 minute)")

  sql <- translate("SELECT dateadd(hour, 1, '2019-01-01 00:00:00')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01 00:00:00' + INTERVAL 1 hour)")

  sql <- translate("SELECT dateadd(d, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT date_add('2019-01-01', 1)")

  sql <- translate("SELECT dateadd(dd, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT date_add('2019-01-01', 1)")

  sql <- translate("SELECT dateadd(day, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT date_add('2019-01-01', 1)")

  sql <- translate("SELECT dateadd(m, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01' + INTERVAL 1 month)")

  sql <- translate("SELECT dateadd(mm, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01' + INTERVAL 1 month)")

  sql <- translate("SELECT dateadd(month, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01' + INTERVAL 1 month)")

  sql <- translate("SELECT dateadd(yy, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01' + INTERVAL 1 year)")

  sql <- translate("SELECT dateadd(yyyy, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01' + INTERVAL 1 year)")

  sql <- translate("SELECT dateadd(year, 1, '2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT ('2019-01-01' + INTERVAL 1 year)")
})


test_that("translate sql server -> spark datediff", {
  sql <- translate("SELECT datediff(d, '2019-01-01', '2019-01-02')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT datediff('2019-01-02', '2019-01-01')")

  sql <- translate("SELECT datediff(dd, '2019-01-01', '2019-01-02')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT datediff('2019-01-02', '2019-01-01')")

  sql <- translate("SELECT datediff(day, '2019-01-01', '2019-01-02')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT datediff('2019-01-02', '2019-01-01')")
})

test_that("translate sql server -> spark convert date", {
  sql <- translate("select convert(varchar,'2019-01-01',112)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select '2019-01-01'")
})

test_that("translate sql server -> spark GETDATE()", {
  sql <- translate("select GETDATE()",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select CURRENT_DATE")
})

test_that("translate sql server -> spark concat", {
  sql <- translate("select 'oh' + 'dsi'",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select 'oh' || 'dsi'")
})

test_that("translate sql server -> spark cast varchar and concat", {
  sql <- translate("select cast('test' as varchar(10)) + 'ing'",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select cast('test' as STRING) || 'ing'")
})

test_that("translate sql server -> spark date from parts", {
  sql <- translate("select datefromparts('2019','01','01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select to_date(cast('2019' as string) || '-' || cast('01' as string) || '-' || cast('01' as string))")
})

test_that("translate sql server -> spark datetime from parts", {
  sql <- translate("select datetimefromparts('2019', '01', '01', '12', '15', '30', '01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select to_timestamp(cast('2019' as string) || '-' || cast('01' as string) || '-' || cast('01' as string) || ' ' || cast('12' as string) || ':' || cast('15' as string) || ':' || cast('30' as string) || '.' || cast('01' as string))")
})

test_that("translate sql server -> spark eomonth", {
  sql <- translate("select eomonth('2019-01-01')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select last_day('2019-01-01')")
})

test_that("translate sql server -> spark stdev", {
  sql <- translate("select STDEV(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select STDDEV(x)")
})

test_that("translate sql server -> spark var", {
  sql <- translate("select VAR(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select VARIANCE(x)")
})

test_that("translate sql server -> spark len", {
  sql <- translate("select LEN(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select LENGTH(x)")
})


test_that("translate sql server -> spark charindex", {
  sql <- translate("select CHARINDEX('test', 'e')",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select INSTR('e', 'test')")
})


test_that("translate sql server -> spark log", {
  sql <- translate("select LOG(x,y)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select LOG(y,x)")

  sql <- translate("select LOG(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select LN(x)")

  sql <- translate("select LOG10(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select LOG(10,x)")
})

test_that("translate sql server -> spark isnull", {
  sql <- translate("select ISNULL(x,y)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select COALESCE(x,y)")
})

test_that("translate sql server -> spark isnumeric", {
  sql <- translate("select ISNUMERIC(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select CASE WHEN CAST ( x AS DOUBLE ) IS NOT NULL THEN 1 ELSE 0 END")
})

test_that("translate sql server -> spark count_big", {
  sql <- translate("select COUNT_BIG(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select COUNT(x)")
})

test_that("translate sql server -> spark square", {
  sql <- translate("select SQUARE(x)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select ((x)*(x))")
})

test_that("translate sql server -> spark NEWID", {
  sql <- translate("select NEWID()",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select UUID()")
})


test_that("translate sql server -> spark if object_id", {
  sql <- translate("IF OBJECT_ID('some_table', 'U') IS NULL CREATE TABLE some_table (id int);",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS some_table  \nUSING DELTA\nAS\nSELECT\nCAST(NULL AS int) AS id  WHERE 1 = 0;")

  sql <- translate("IF OBJECT_ID('some_table', 'U') IS NOT NULL DROP TABLE some_table;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS some_table;")
})

test_that("translate sql server -> spark dbo", {
  sql <- translate("select * from cdm.dbo.test",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select * from cdm.test")
})


test_that("translate sql server -> spark table admin", {
  sql <- translate("CREATE CLUSTERED INDEX index_name ON some_table (variable);",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "")

  sql <- translate("CREATE UNIQUE CLUSTERED INDEX index_name ON some_table (variable);",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "")

  sql <- translate("PRIMARY KEY NONCLUSTERED",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "")

  sql <- translate("UPDATE STATISTICS test;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "")
})

test_that("translate sql server -> spark datetime", {
  sql <- translate("DATETIME",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "TIMESTAMP")

  sql <- translate("DATETIME2",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "TIMESTAMP")
})


test_that("translate sql server -> spark varchar", {
  sql <- translate("VARCHAR(MAX)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "STRING")

  sql <- translate("VARCHAR",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "STRING")

  sql <- translate("VARCHAR(100)",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "STRING")
})

test_that("translate sql server -> spark cte ctas", {
  sql <- translate(
    sql = "WITH a AS (select b) SELECT c INTO d FROM e;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "DROP VIEW IF EXISTS a ; CREATE TEMPORARY VIEW a  AS (select b);\n CREATE TABLE d \nUSING DELTA\nAS\n(SELECT\nc \nFROM\ne);")
})

test_that("translate sql server -> spark ctas", {
  sql <- translate(
    sql = "SELECT a INTO b FROM c;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE b \n USING DELTA \n  AS\nSELECT\na \nFROM\nc;")
})

test_that("translate sql server -> spark ctas with distribute_on_key", {
  sql <- translate(
    sql = "--HINT DISTRIBUTE_ON_KEY(key)
                          SELECT a INTO b FROM c;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "--HINT DISTRIBUTE_ON_KEY(key) \nCREATE TABLE b \nUSING DELTA\nAS\nSELECT\na \nFROM\nc;\nOPTIMIZE b  ZORDER BY key;")
})

test_that("translate sql server -> spark cross join", {
  sql <- translate(
    sql = "SELECT a from (select b) x, (select c) y;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "SELECT a  FROM (select b) x cross join (select c) y;")
})

test_that("translate sql server -> spark DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "spark")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test;")
})

test_that("translate sql server -> spark double CTE INSERT INTO", {
  sql <- translate(
    "WITH a AS (
    SELECT * FROM my_table_1
  ), b AS (
    SELECT * FROM my_table_2
  )
  SELECT *
  INTO some_table
  FROM a, b;",
    targetDialect = "spark"
  )
  expect_equal_ignore_spaces(
    sql,
    "DROP VIEW IF EXISTS a  ; CREATE TEMPORARY VIEW a   AS (SELECT * FROM my_table_1\n );\nDROP VIEW IF EXISTS b ; CREATE TEMPORARY VIEW b  AS (SELECT * FROM my_table_2\n );\n CREATE TABLE some_table \nUSING DELTA\nAS\n(SELECT\n*\nFROM\na, b);"
  )
})

test_that("translate sql server -> spark IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "spark")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table ;")
})

test_that("translate sql server -> spark DATEPART", {
  sql <- translate("select DATEPART(YEAR, some_date) from my_table",
                   targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select DATE_PART('YEAR', some_date) from my_table")
})

test_that("translate sql server -> spark DATEADD DAY with float", {
  sql <- translate("select DATEADD(DAY, 1.0, some_date) from my_table;",
                   targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select date_add(some_date, 1) from my_table;")
})

test_that("translate sql server -> spark DATEADD YEAR with float", {
  sql <- translate("select DATEADD(YEAR, 1.0, some_date) from my_table;",
                   targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "select (some_date + INTERVAL 1 year) from my_table;")
})

test_that("translate sql server -> spark DATEADD YEAR with float", {
  sql <- translate("WITH cte AS (SELECT * FROM table) SELECT * INTO tmp.table FROM cte;",
                   targetDialect = "spark"
  )
  expect_equal_ignore_spaces(sql, "DROP VIEW IF EXISTS cte ; CREATE TEMPORARY VIEW cte  AS (SELECT * FROM table);\n CREATE TABLE tmp.table \nUSING DELTA\nAS\n(SELECT\n* \nFROM\ncte);")
})

test_that("translate sql server -> spark temp table field ref", {
  sql <- translate("SELECT #tmp.name FROM #tmp;", targetDialect = "spark", tempEmulationSchema = "ts")
  expect_equal_ignore_spaces(sql, sprintf("SELECT %stmp.name FROM ts.%stmp;", getTempTablePrefix(), getTempTablePrefix()))
})
