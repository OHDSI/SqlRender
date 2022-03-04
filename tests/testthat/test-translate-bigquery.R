library("testthat")

# For debugging: force reload of patterns:
# rJava::J('org.ohdsi.sql.SqlTranslate')$setReplacementPatterns('inst/csv/replacementPatterns.csv')

expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equal(string1, string2)
}

test_that("translate sql server -> Big Query select random row using hash", {
  sql <- translate("SELECT column FROM (SELECT column, ROW_NUMBER() OVER (ORDER BY HASHBYTES('MD5',CAST(person_id AS varchar))) tmp WHERE rn <= 1",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select column from (select column, row_number() over (order by md5(cast(person_id as STRING))) tmp where rn <= 1"
  )
})

test_that("translate sql server -> BigQuery SELECT CONVERT(VARBINARY, @a, 1)", {
  sql <- translate("SELECT ROW_NUMBER() OVER CONVERT(VARBINARY, val, 1) rn WHERE rn <= 1",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select row_number() over safe_cast(concat('0x', val) as int64) rn where rn <= 1"
  )
})

test_that("translate sql server -> bigquery lowercase all but strings and variables", {
  sql <- translate("SELECT X.Y, 'Mixed Case String' FROM \"MixedCaseTableName.T\" where x.z=@camelCaseVar GROUP BY X.Y",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select x.y, 'Mixed Case String' from \"MixedCaseTableName.T\" where x.z=@camelCaseVar group by x.y"
  )
})

test_that("translate sql server -> bigquery common table expression column list", {
  sql <- translate("with cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "with cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;"
  )
})

test_that("translate sql server -> bigquery common table expression column list no from or union", {
  sql <- translate("WITH data(x) AS (SELECT (CAST(1 AS INT) x)) SELECT x INTO my_table FROM data;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE my_table  AS WITH data  as (select (cast(1  as int64) x) as x)  SELECT x  FROM data;"
  )
})

test_that("translate sql server -> bigquery multiple common table expression column list", {
  sql <- translate("with cte1 as (select 2), cte(x, y, z) as (select c1, c2 as y, c3 as r from t) select x, y, z from cte;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "with cte1 as (select 2), cte as (select c1 as x, c2 as y, c3 as z from t) select x, y, z from cte;"
  )
})

test_that("translate sql server -> bigquery distinct keyword", {
  sql <- translate("with cte2 (column1, column2) as (select distinct c1.column1, c1.column2 from cte c1) select column1, column2 into cte2 from cte2",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "with cte2  as (select distinct c1.column1 as column1,c1.column2  as column2 from cte c1) select column1, column2 into cte2 from cte2"
  )
})

test_that("translate sql server -> bigquery group by function", {
  sql <- translate("select f(a), count(*) from t group by f(a);", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select f(a), count(*) from t group by 1;")
})

test_that("translate sql server -> bigquery group by addition", {
  sql <- translate("select 100, sum(x), cast(a+b as string) from t group by a+b;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(sql, "select 100, sum(x), cast(a+b as string) from t group by 3;")
})

test_that("translate sql server -> bigquery column ref groupby", {
  sql <- translate("select 100, sum(x), cast(a+b as string) from t group by t.a, t.b;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select 100, sum(x), cast(a+b as string) from t group by t.a, t.b;"
  )
})

test_that("translate sql server -> bigquery group by without match", {
  sql <- translate("select 100, sum(x), concat('count = ', c) from t group by a+b;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select 100, sum(x), concat('count = ', c) from t group by a + b;"
  )
})

test_that("translate sql server -> bigquery group by without final semicolon", {
  sql <- translate("select f(a) from t group by f(a);", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select f(a) from t group by 1;")
})

test_that("translate sql server -> bigquery order by", {
  sql <- translate("select f(a) from t group by f(a) order by f(a);", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select f(a) from t group by 1 order by 1;")
})

test_that("translate sql server -> bigquery nested group by", {
  sql <- translate("select * from (select 100, cast(a+b as string), max(x) from t group by a+b) dt;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select * from (select 100, cast(a+b as string), max(x) from t group by 2) dt;"
  )
})

test_that("translate sql server -> bigquery complex group by", {
  sql <- translate("select 100, 200, cast(floor(date_diff(a, b, day)/30) string string), 300 from t group by floor(date_diff(a, b, day)/30);",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select 100, 200, cast(floor(date_diff(a, b, day)/30) string string), 300 from t group by 3;"
  )
})

test_that("translate sql server -> bigquery group by having", {
  sql <- translate(paste(
    "select cast(stratum_1 as integer) as concept_id, sum(count_value) as count_value ",
    "from heracles_results ",
    "where analysis_id in (123) ",
    "group by cast(stratum_1 as integer) ",
    "having sum(count_value) > 1;"
  ), targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    paste(
      "select cast(stratum_1 as INT64) as concept_id, sum(count_value) as count_value ",
      "from heracles_results ",
      "where analysis_id in (123) ",
      "group by 1 ",
      "having sum(count_value) > 1;"
    )
  )
})

test_that("translate sql server -> bigquery column references", {
  sql <- translate("select concat(t.a, t.b) from t group by t.a, t.b;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select concat(t.a, t.b) from t group by t.a, t.b;")
})

test_that("translate sql server -> bigquery mixed column references", {
  sql <- translate("select concat(t.a, t.b), x+y+z from t group by t.a, t.b, x+y+z;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(sql, "select concat(t.a, t.b), x+y+z from t group by t.a, t.b, 2;")
})

test_that("translate sql server -> bigquery DATEDIFF", {
  sql <- translate("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select DATE_DIFF(IF(SAFE_CAST(drug_era_end_date AS DATE) IS NULL,PARSE_DATE('%Y%m%d',cast(drug_era_end_date AS STRING)),SAFE_CAST(drug_era_end_date AS DATE)),IF(SAFE_CAST(drug_era_start_date AS DATE) IS NULL,PARSE_DATE('%Y%m%d',cast(drug_era_start_date AS STRING)),SAFE_CAST(drug_era_start_date AS DATE)),DAY)from drug_era;"
  )
})

test_that("translate sql server -> bigquery DATEDIFF year", {
  sql <- translate("SELECT DATEDIFF(YEAR,drug_era_start_date,drug_era_end_date) FROM drug_era;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select (EXTRACT(YEAR from IF(SAFE_CAST(drug_era_end_date  AS DATE) IS NULL,PARSE_DATE('%Y%m%d', cast(drug_era_end_date  AS STRING)),SAFE_CAST(drug_era_end_date  AS DATE))) - EXTRACT(YEAR from IF(SAFE_CAST(drug_era_start_date  AS DATE) IS NULL,PARSE_DATE('%Y%m%d', cast(drug_era_start_date  AS STRING)),SAFE_CAST(drug_era_start_date  AS DATE)))) from drug_era;"
  )
})

test_that("translate sql server -> bigquery DATEADD", {
  sql <- translate("SELECT DATEADD(dd,30,drug_era_end_date) FROM drug_era;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select DATE_ADD(IF(SAFE_CAST(drug_era_end_date AS DATE) IS NULL,PARSE_DATE('%Y%m%d',cast(drug_era_end_date AS STRING)),SAFE_CAST(drug_era_end_date AS DATE)), interval 30 DAY) from drug_era;"
  )
})

test_that("translate sql server -> bigquery GETDATE", {
  sql <- translate("GETDATE()", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "CURRENT_DATE()")
})

test_that("translate sql server -> bigquery STDEV", {
  sql <- translate("stdev(x)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "STDDEV(x)")
})

test_that("translate sql server -> bigquery LEN", {
  sql <- translate("len('abc')", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "LENGTH('abc')")
})

test_that("translate sql server -> bigquery COUNT_BIG", {
  sql <- translate("COUNT_BIG(x)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "COUNT(x)")
})

test_that("translate sql server -> bigquery CAST varchar", {
  sql <- translate("select cast(x as varchar)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select cast(x as STRING)")
})

test_that("translate sql server -> bigquery CAST :float", {
  sql <- translate("select cast(x as:float)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select CAST(x as float64)")
})

test_that("translate sql server -> bigquery DROP TABLE IF EXISTS", {
  sql <- translate("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(sql, "DROP TABLE IF EXISTS cohort;")
})

test_that("translate sql server -> bigquery CAST string", {
  sql <- translate("CAST(x AS VARCHAR(255))", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(x as STRING)")
})

test_that("translate sql server -> bigquery LEFT, RIGHT", {
  sql <- translate("select LEFT(a, 20), RIGHT(b, 30) FROM t;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select SUBSTR(a, 0, 20), SUBSTR(b, -30) from t;")
})

test_that("translate sql server -> bigquery cast float", {
  sql <- translate("cast(a as float)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(a as float64)")
})

test_that("translate sql server -> bigquery cast bigint", {
  sql <- translate("cast(a as bigint)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(a as int64)")
})

test_that("translate sql server -> bigquery cast int", {
  sql <- translate("cast(a as int)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "cast(a as int64)")
})

test_that("translate sql server -> bigquery cast date", {
  sql <- translate("date(d)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "IF(SAFE_CAST(d AS DATE) IS NULL,PARSE_DATE('%Y%m%d',cast(d AS STRING)),SAFE_CAST(d AS DATE))"
  )
})

test_that("translate sql server -> bigquery cast concat string as date", {
  sql <- translate("cast(concat(a,b) as date)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "parse_date('%Y%m%d', concat(a,b))")
})

test_that("translate sql server -> bigquery cast string as date", {
  sql <- translate("cast(a as date)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "IF(SAFE_CAST(a AS DATE) IS NULL,PARSE_DATE('%Y%m%d',cast(a AS STRING)),SAFE_CAST(a AS DATE))"
  )
})

test_that("translate sql server -> bigquery extract year", {
  sql <- translate("year(d)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "EXTRACT(YEAR from d)")
})

test_that("translate sql server -> bigquery extract month", {
  sql <- translate("month(d)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "EXTRACT(MONTH from d)")
})

test_that("translate sql server -> bigquery extract day", {
  sql <- translate("day(d)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "EXTRACT(DAY from d)")
})

test_that("translate sql server -> bigquery union distinct", {
  sql <- translate("select 1 as x union select 2;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select 1 as x union distinct select 2;")
})

test_that("translate sql server -> bigquery intersect distinct", {
  sql <- translate("SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2;"
  )
})

test_that("translate sql server -> bigquery bracketed intersect distinct", {
  sql <- translate("(SELECT DISTINCT a FROM t INTERSECT SELECT DISTINCT a FROM s)",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "(SELECT t1.a FROM (SELECT DISTINCT a FROM t UNION ALL SELECT DISTINCT a FROM s) AS t1 GROUP BY a HAVING COUNT(*) >= 2)"
  )
})

test_that("translate sql server -> bigquery isnull", {
  sql <- translate("SELECT isnull(x,y) from t;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select IFNULL(x,y) from t;")
})

test_that("translate sql server -> bigquery unquote aliases", {
  sql <- translate("SELECT a as \"b\" from t;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select a as b from t;")
})

test_that("translate sql server -> bigquery cast int in coalesce", {
  sql <- translate("select coalesce(x, 0), coalesce(12, y) from t", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "select coalesce(cast(x as int64), 0), coalesce(12, cast(y as int64)) from  t"
  )
})

test_that("translate sql server -> bigquery cast decimal", {
  sql <- translate("select cast(x as decimal(18,4)) from t", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select cast(x as float64) from t")
})

test_that("translate sql server -> bigquery ISNUMERIC", {
  sql <- translate("select ISNUMERIC(a) from b", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "select CASE WHEN SAFE_CAST(a AS FLOAT64) IS NULL THEN 0 ELSE 1 END from b"
  )
  sql <- translate("select a FROM table WHERE ISNUMERIC(a) = 1", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "select a from table where CASE WHEN SAFE_CAST(a AS FLOAT64) IS NULL THEN 0 ELSE 1 END = 1"
  )
  sql <- translate("select a FROM table WHERE ISNUMERIC(a) = 0", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "select a from table where CASE WHEN SAFE_CAST(a AS FLOAT64) IS NULL THEN 0 ELSE 1 END = 0"
  )
})

test_that("translate sql server -> bigquery index not supported", {
  sql <- translate("CREATE INDEX idx_raw_4000 ON #raw_4000 (cohort_definition_id, subject_id, op_start_date);",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(sql, "-- bigquery does not support indexes")
})

test_that("translate sql server -> bigquery TRUNCATE TABLE", {
  sql <- translate("TRUNCATE TABLE cohort;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "DELETE FROM cohort WHERE True;")
})

test_that("translate sql server -> bigquery DATEFROMPARTS", {
  sql <- translate("select DATEFROMPARTS(2019,1,30)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "select DATE(2019,1,30)")
})

test_that('translate sql server -> bigquery offset literal'{
  sql <- translate('create table test_that ("offset" STRING);', targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "create table test_that (offset STRING);")
})

test_that("translate sql server -> bigquery EOMONTH()", {
  sql <- translate("select eomonth(payer_plan_period_start_date)", targetDialect = "bigquery")
  expect_equal_ignore_spaces(
    sql,
    "select DATE_SUB(DATE_TRUNC(DATE_ADD(payer_plan_period_start_date, INTERVAL 1 MONTH), MONTH), INTERVAL 1 DAY)"
  )
})

test_that("translate sql server -> bigquery escape chars", {
  sql <- translate("INSERT INTO t VALUES('some \"string\" ''with escape'' chars')",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "insert into t values(CONCAT('some \\042string\\042 ','\\047','with escape','\\047','chars'))"
  )
})

test_that("translate SELECT INTO + CTE bigquery", {
  sql <- translate("WITH data (a,b) AS (SELECT 1, 2 UNION ALL SELECT 3, 4) SELECT a,b INTO test FROM data;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "CREATE TABLE test AS WITH data as (select 1 as a, 2 as b union all select 3, 4) SELECT a,b FROM data;"
  )
})

test_that("translate sql server -> BigQuery UPDATE STATISTICS", {
  sql <- translate("UPDATE STATISTICS results_schema.heracles_results;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "-- big query does not support such functionality")
})

test_that("translate sql server -> BigQuery modulus", {
  sql <- translate("SELECT row_number() over (order by cast(person_id % 123 as int))",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select row_number() over (order by CAST(MOD(person_id, 123) AS INT64))"
  )
  sql <- translate("SELECT row_number() over (order by cast((person_id % 123) as int))",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select row_number() over (order by CAST(MOD(person_id, 123) AS INT64))"
  )
})

test_that("translate sql server -> BigQuery % operator", {
  sql <- translate("SELECT  (CAST(person_id*month(cohort_start_date) AS BIGINT) % 123)*(CAST(year(cohort_start_date)*day(cohort_start_date) AS BIGINT) % 123)) FROM my_table;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select (MOD(cast(person_id*EXTRACT(MONTH from cohort_start_date) as int64), 123))*(MOD(cast(EXTRACT(YEAR from cohort_start_date)*EXTRACT(DAY from cohort_start_date) as int64), 123))) from my_table;"
  )
})

test_that("translate sql server -> String concatenation", {
  sql <- translate("SELECT last_name + ', ' + first_name FROM my_table;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select CONCAT(last_name, ', ', first_name) from my_table;"
  )
})

test_that("translate sql server -> String concatenation", {
  sql <- translate("SELECT first_name + CAST(middle_initial AS VARCHAR) + last_name FROM my_table;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select CONCAT(first_name, CAST(middle_initial AS STRING), last_name) from my_table;"
  )
})

test_that("translate sql server -> String concatenation", {
  sql <- translate("SELECT first_name + CAST(middle_initial AS VARCHAR(1)) + last_name FROM my_table;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select CONCAT(first_name, CAST(middle_initial AS STRING), last_name) from my_table;"
  )
})

test_that("translate sql server -> String concatenation", {
  sql <- translate("SELECT subgroup_id, 'Persons aged ' + cast(age_low as varchar) + ' to ' + cast(age_high as varchar) + ' with gender = ' + gender_name FROM subgroups;",
    targetDialect = "bigquery"
  )
  expect_equal_ignore_spaces(
    sql,
    "select subgroup_id, CONCAT('Persons aged ', CONCAT(cast(age_low as STRING), 'to ', cast(age_high as STRING), 'with gender = '), gender_name) from subgroups;"
  )
})

test_that("translate sql server -> bigquery DROP TABLE IF EXISTS", {
  sql <- translate("DROP TABLE IF EXISTS test;", targetDialect = "bigquery")
  expect_equal_ignore_spaces(sql, "drop table if exists test;")
})
