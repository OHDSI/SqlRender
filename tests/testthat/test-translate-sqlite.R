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

test_that("translate sql server -> SQLite string concat", {
  sql <- translate("'x' + b ( 'x' + b)", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "'x' || b ( 'x' || b)")
})

test_that("translate sql server -> SQLite string concat", {
  sql <- translate("a + ';b'", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "a || ';b'")
})

test_that("translate sql server -> SQLite string concat", {
  sql <- translate("a + ';('", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "a || ';('")
})

test_that("translate sql server -> SQLite add month", {
  sql <- translate("DATEADD(mm,1,date)", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "CAST(STRFTIME('%s', DATETIME(date, 'unixepoch', (1)||' months')) AS REAL)"
  )
})

test_that("translate sql server -> SQLite WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "sqlite"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE d \nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> SQLite WITH SELECT INTO without FROM", {
  sql <- translate("SELECT c INTO d;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "CREATE TABLE d AS\nSELECT\nc ;")
})

test_that("translate sql server -> SQLite WITH INSERT INTO SELECT", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;",
    targetDialect = "sqlite"
  )
  expect_equal_ignore_spaces(
    sql,
    "WITH cte1 AS (SELECT a FROM b) INSERT INTO c (d int) SELECT e FROM cte1;"
  )
})

test_that("translate sql server -> SQLite create table if not exists", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NULL\n CREATE TABLE cohort\n(cohort_definition_id INT);",
    targetDialect = "sqlite"
  )
  expect_equal_ignore_spaces(sql, "CREATE TABLE IF NOT EXISTS cohort\n (cohort_definition_id INT);")
})

test_that("translate sql server -> SQLite select random row", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM table) tmp WHERE rn <= 1",
    targetDialect = "sqlite"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY ((RANDOM()+9223372036854775808) / 18446744073709551615)) AS rn FROM table) tmp WHERE rn <= 1"
  )
})

test_that("translate sql server -> SQLite temp table", {
  sql <- translate("SELECT * FROM #my_temp;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT * FROM temp.my_temp;")
})

test_that("translate sql server -> sqliteql TOP", {
  sql <- translate("SELECT TOP 10 * FROM my_table WHERE a = b;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT * FROM my_table WHERE a = b LIMIT 10;")
})

test_that("translate sql server -> sqliteql TOP subquery", {
  sql <- translate("SELECT name FROM (SELECT TOP 1 name FROM my_table WHERE a = b);",
    targetDialect = "sqlite"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT name FROM (SELECT name FROM my_table WHERE a = b LIMIT 1);"
  )
})

test_that("translate sql server -> sqlite date to string", {
  sql <- translate("SELECT CONVERT(VARCHAR,start_date,112) FROM table;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT CAST(STRFTIME('%Y%m%d', start_date) AS REAL) FROM table;")
})

test_that("translate sql server -> sqlite CONVERT(AS DATE)", {
  sql <- translate("CONVERT(DATE, '20000101');", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "CAST(STRFTIME('%s', SUBSTR(CAST('20000101' AS TEXT), 1, 4) || '-' || SUBSTR(CAST('20000101' AS TEXT), 5, 2) || '-' || SUBSTR(CAST('20000101' AS TEXT), 7)) AS REAL);"
  )
})

test_that("translate sql server -> sqlite CONVERT(AS DATE)", {
  sql <- translate("CAST('20000101' AS DATE);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "CAST(STRFTIME('%s', SUBSTR(CAST('20000101' AS TEXT), 1, 4) || '-' || SUBSTR(CAST('20000101' AS TEXT), 5, 2) || '-' || SUBSTR(CAST('20000101' AS TEXT), 7)) AS REAL);"
  )
})

test_that("translate sql server -> sqlite log any base", {
  sql <- translate("SELECT LOG(number, base) FROM table", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT (LOG(number)/LOG(base)) FROM table")
})

test_that("translate sql server -> sqlite ISNUMERIC", {
  sql <- translate("SELECT CASE WHEN ISNUMERIC(a) = 1 THEN a ELSE b FROM c;",
    targetDialect = "sqlite"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT CASE WHEN CASE WHEN a GLOB '[0-9]*' OR a GLOB '[0-9]*.[0-9]*' OR a GLOB '.[0-9]*' THEN 1 ELSE 0 END = 1 THEN a ELSE b FROM c;"
  )
  sql <- translate("SELECT a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT a FROM table WHERE CASE WHEN a GLOB '[0-9]*' OR a GLOB '[0-9]*.[0-9]*' OR a GLOB '.[0-9]*' THEN 1 ELSE 0 END = 1"
  )
})

test_that("translate sql server -> SQLite analyze table", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "ANALYZE results_schema.heracles_results;")
})

test_that("translate sql server -> SQLite DATETIME and DATETIME2", {
  sql <- translate("CREATE TABLE x (a DATETIME2, b DATETIME);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "CREATE TABLE x (a REAL, b REAL);")
})

test_that("translate sql server -> sqlite GETDATE", {
  sql <- translate("GETDATE()", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "STRFTIME('%s','now')")
})

test_that("translate sql server -> sqlite CREATE INDEX", {
  sql <- translate("CREATE INDEX idx_1 ON main.person (person_id);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "CREATE INDEX idx_1  ON person  (person_id);")
})

test_that("translate sql server -> sqlite DATEDIFF with literals", {
  sql <- translate("SELECT DATEDIFF(DAY, '20000131', '20000101');", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT JULIANDAY(CAST(STRFTIME('%s', SUBSTR(CAST('20000101' AS TEXT), 1, 4) || '-' || SUBSTR(CAST('20000101' AS TEXT), 5, 2) || '-' || SUBSTR(CAST('20000101' AS TEXT), 7)) AS REAL), 'unixepoch') - JULIANDAY(CAST(STRFTIME('%s', SUBSTR(CAST('20000131' AS TEXT), 1, 4) || '-' || SUBSTR(CAST('20000131' AS TEXT), 5, 2) || '-' || SUBSTR(CAST('20000131' AS TEXT), 7)) AS REAL), 'unixepoch');"
  )
})

test_that("translate sql server -> sqlite DATEDIFF with date fields", {
  sql <- translate("SELECT DATEDIFF(DAY, date1, date2);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT JULIANDAY(date2, 'unixepoch') - JULIANDAY(date1, 'unixepoch');"
  )
})

test_that("translate sql server -> sqlite DATEDIFF year with literals", {
  sql <- translate("SELECT DATEDIFF(YEAR, '20010131', '20000101');", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (CAST(SUBSTR('20000101', 1, 4) AS REAL) - CAST(SUBSTR('20010131', 1, 4) AS REAL));"
  )
})

test_that("translate sql server -> sqlite DATEDIFF year with date fields", {
  sql <- translate("SELECT DATEDIFF(YEAR, date1, date2);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT (STRFTIME('%Y', date2, 'unixepoch') - STRFTIME('%Y', date1, 'unixepoch'));"
  )
})

test_that("translate sql server -> sqlite DATEDIFF month literals", {
  sql <- translate("SELECT DATEDIFF(MONTH, '20000115', '20010116');", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT ((CAST(SUBSTR('20010116', 1, 4) AS REAL)*12 + CAST(SUBSTR('20010116', 5, 2) AS REAL)) - (CAST(SUBSTR('20000115', 1, 4) AS REAL)*12 + CAST(SUBSTR('20000115', 5, 2) AS REAL)) + (CASE WHEN CAST(SUBSTR('20010116', 7, 2) AS REAL) >= CAST(SUBSTR('20000115', 7, 2) AS REAL) then 0 else -1 end));"
  )
})

test_that("translate sql server -> sqlite DATEDIFF monthdate fields", {
  sql <- translate("SELECT DATEDIFF(MONTH, date1, date2);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(
    sql,
    "SELECT ((STRFTIME('%Y', date2, 'unixepoch')*12 + STRFTIME('%m', date2, 'unixepoch')) - (STRFTIME('%Y', date1, 'unixepoch')*12 + STRFTIME('%m', date1, 'unixepoch')) + (CASE WHEN STRFTIME('%d', date2, 'unixepoch') >= STRFTIME('%d', date1, 'unixepoch') then 0 else -1 end));"
  )
})

test_that("translate sql server -> sqlite CEILING", {
  sql <- translate("SELECT CEILING(0.1);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT CEIL(0.1);")
})

test_that("translate sql server -> sqlite DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS test;")
})

test_that("translate sql server -> sqlite IIF", {
  sql <- translate("SELECT IIF(a>b, 1, b) AS max_val FROM table;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT CASE WHEN a>b THEN 1 ELSE b END AS max_val FROM table ;")
})

test_that("translate sql server -> sqlite UNION ALL parentheses", {
  sql <- translate("SELECT * FROM ((SELECT * FROM a) UNION ALL (SELECT * FROM b));", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT * FROM (SELECT * FROM a UNION ALL SELECT * FROM b);")
})

test_that("translate sql server -> sqlite UNION parentheses", {
  sql <- translate("SELECT * FROM ((SELECT * FROM a) UNION (SELECT * FROM b));", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT * FROM (SELECT * FROM a UNION SELECT * FROM b);")
})

test_that("translate sql server -> sqlite UNION parentheses with IN", {
  sql <- translate("SELECT * FROM x WHERE y IN (SELECT * FROM a) UNION SELECT * FROM z;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT * FROM x WHERE y IN ((SELECT * FROM a)) UNION SELECT * FROM z;")
})


test_that("translate sql server -> sqlite TRY_CAST", {
  sql <- translate("SELECT TRY_CAST(x AS INT) FROM x;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT CAST(x AS INT) FROM x;")
})

test_that("translate sql server -> sqlite drvd()", {
  sql <- translate("SELECT
      TRY_CAST(name AS VARCHAR(MAX)) AS name,
      TRY_CAST(speed AS FLOAT) AS speed
    FROM (  VALUES ('A', 1.0), ('B', 2.0)) AS drvd(name, speed);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT
      CAST(name AS TEXT) AS name,
      CAST(speed AS REAL) AS speed
    FROM (SELECT NULL AS name, NULL AS speed WHERE (0 = 1) UNION ALL VALUES ('A', 1.0), ('B', 2.0)) AS values_table;")
})

test_that("translate sql server -> sqlite temp table field ref", {
  sql <- translate("SELECT #tmp.name FROM #tmp;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "SELECT tmp.name FROM temp.tmp;")
})

test_that("translate sql server -> sqlite ALTER TABLE ADD single", {
  sql <- translate("ALTER TABLE my_table ADD a INT;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "ALTER TABLE my_table  ADD a INT;")
})

test_that("translate sql server -> sqlite ALTER TABLE ADD multiple", {
  sql <- translate("ALTER TABLE my_table ADD a INT, b INT, c VARCHAR(255);", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "ALTER TABLE my_table ADD a INT; ALTER TABLE my_table ADD b INT; ALTER TABLE my_table ADD c TEXT;")
})

test_that("translate sql server -> sqlite ALTER TABLE ADD COLUMN", {
  # Note: this is incorrect OhdsiSql, but included for legacy reasons (https://github.com/OHDSI/CohortDiagnostics/issues/1080)
  sql <- translate("ALTER TABLE my_table ADD COLUMN a INT;", targetDialect = "sqlite")
  expect_equal_ignore_spaces(sql, "ALTER TABLE my_table ADD COLUMN a INT;")
})
