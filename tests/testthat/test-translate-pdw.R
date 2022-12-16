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

test_that("translate sql server -> PDW WITH SELECT INTO", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO d FROM cte1;",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE d WITH (DISTRIBUTION = REPLICATE)\nAS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> PDW WITH SELECT INTO temp table", {
  sql <- translate("WITH cte1 AS (SELECT a FROM b) SELECT c INTO #d FROM cte1;",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE  #d   WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte1 AS (SELECT a FROM b)  SELECT\nc \nFROM\ncte1;"
  )
})

test_that("translate sql server -> PDW create temp table", {
  sql <- translate("CREATE TABLE #a (x int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a (x int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE);"
  )
})

test_that("translate sql server -> PDW create temp table with person_id", {
  sql <- translate("CREATE TABLE #a (person_id int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( person_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(person_id));"
  )
})

test_that("translate sql server -> PDW create temp table with subject_id", {
  sql <- translate("CREATE TABLE #a (subject_id int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( subject_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(subject_id));"
  )
})

test_that("translate sql server -> PDW create temp table with analysis_id", {
  sql <- translate("CREATE TABLE #a (analysis_id int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE #a ( analysis_id int)\nWITH (LOCATION = USER_DB, DISTRIBUTION = HASH(analysis_id));"
  )
})

test_that("translate sql server -> PDW create permanent table", {
  sql <- translate("CREATE TABLE a (x int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (x int)\nWITH (DISTRIBUTION = REPLICATE);"
  )
})

test_that("translate sql server -> PDW create permanent table with person_id", {
  sql <- translate("CREATE TABLE a (person_id int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( person_id int)\nWITH (DISTRIBUTION = HASH(person_id));"
  )
})

test_that("translate sql server -> PDW create permanent table with subject_id", {
  sql <- translate("CREATE TABLE a (subject_id int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( subject_id int)\nWITH (DISTRIBUTION = HASH(subject_id));"
  )
})

test_that("translate sql server -> PDW create permanent table with analysis_id", {
  sql <- translate("CREATE TABLE a (analysis_id int);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a ( analysis_id int)\nWITH (DISTRIBUTION = HASH(analysis_id));"
  )
})

test_that("translate sql server -> PDW select into permanent table", {
  sql <- translate("SELECT a INTO b FROM c WHERE a = 1;", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n a \nFROM\n c WHERE a = 1;"
  )
})

test_that("translate sql server -> PDW select into permanent table with person_id", {
  sql <- translate("SELECT a, person_id, b INTO b FROM c WHERE a = 1;", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = HASH(person_id))\nAS\nSELECT\n a, person_id, b \nFROM\n c WHERE a = 1;"
  )
})

test_that("translate sql server -> PDW select into permanent table with analysis_id", {
  sql <- translate("SELECT a, analysis_id, b INTO b FROM c WHERE a = 1;", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE b WITH (DISTRIBUTION = HASH(analysis_id))\nAS\nSELECT\n a, analysis_id, b \nFROM\n c WHERE a = 1;"
  )
})

test_that("translate sql server -> PDW CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 DATETIME CONSTRAINT a_c1_def DEFAULT GETDATE());",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (c1 DATETIME)\nWITH (DISTRIBUTION = REPLICATE);"
  )
})

test_that("translate sql server -> PDW CREATE TABLE with CONSTRAINT DEFAULT", {
  sql <- translate("CREATE TABLE a(c1 DATETIME DEFAULT GETDATE());", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE a (c1 DATETIME)\nWITH (DISTRIBUTION = REPLICATE);"
  )
})

test_that("translate sql server -> PDW CREATE INDEX with WHERE", {
  sql <- translate("CREATE INDEX idx_a ON a(c1, c2) WHERE c3 <> '';", targetDialect = "pdw")
  expect_equal_ignore_spaces(sql, "CREATE INDEX idx_a ON a(c1, c2);")
})

test_that("translate sql server -> PDW cte with preceding 'with' in quotes", {
  sql <- translate("insert into x (a) values ('with'); with cte (a) as(select a from b) select a INTO #c from cte;",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "insert into x (a) values ('with'); IF XACT_STATE() = 1 COMMIT; CREATE TABLE #c WITH (LOCATION = USER_DB, DISTRIBUTION = REPLICATE) AS\nWITH cte (a) AS (select a from b) SELECT\n a \nFROM\n cte;"
  )
})

test_that("translate select into issue for pdw", {
  sql <- "SELECT @c1 INTO table FROM @c2 WHERE a = 1;"
  sql <- translate(sql, targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE table WITH (DISTRIBUTION = REPLICATE)\nAS\nSELECT\n @c1 \nFROM\n @c2 WHERE a = 1;"
  )
})

test_that("translate sql server -> pdw hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;"
  )
})

test_that("translate sql server -> pdw hint distribute_on_key", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id)\nCREATE TABLE(row_id INT);",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE (row_id INT)\nWITH (DISTRIBUTION = HASH(row_id));"
  )
})

test_that("translate sql server -> pdw hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_RANDOM\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = ROUND_ROBIN) AS\nSELECT\n * \nFROM\n other_table;"
  )
})

test_that("translate sql server -> pdw hint distribute_on_random", {
  sql <- translate("--HINT DISTRIBUTE_ON_RANDOM\nCREATE TABLE(row_id INT);", targetDialect = "pdw")
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_RANDOM\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE (row_id INT)\nWITH (DISTRIBUTION = ROUND_ROBIN);"
  )
})


test_that("translate sql server -> PDW CREATE TABLE person_id", {
  sql <- translate("CREATE TABLE [dbo].[drug_era] ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL);",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; CREATE TABLE   [dbo].[drug_era]  ([drug_era_id] bigint NOT NULL, [person_id] bigint NOT NULL, [drug_concept_id] bigint NOT NULL, [drug_era_start_date] date NOT NULL, [drug_era_end_date] date NOT NULL, [drug_exposure_count] int NULL, [gap_days] int NULL)\nWITH (DISTRIBUTION = HASH(person_id));"
  )
})

test_that("translate sql server -> pdw hint DISTKEY + SORTKEY on CREATE TABLE", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nCREATE TABLE my_table (row_id INT, start_date DATE);",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE my_table (row_id INT, start_date DATE)\nWITH (DISTRIBUTION = HASH(row_id));"
  )
})

test_that("translate sql server -> pdw hint DISTKEY + SORTKEY on CTAS", {
  sql <- translate("--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nSELECT * INTO #my_table FROM other_table;",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "--HINT DISTRIBUTE_ON_KEY(row_id) SORT_ON_KEY(start_date)\nIF XACT_STATE() = 1 COMMIT; CREATE TABLE #my_table WITH (LOCATION = USER_DB, DISTRIBUTION = HASH(row_id)) AS\nSELECT\n * \nFROM\n other_table;"
  )
})

test_that("translate create table if not exists pdw", {
  sql <- translate("IF OBJECT_ID('test.testing', 'U') IS NULL create table test.testing (id int);",
    targetDialect = "pdw"
  )
  expect_equal_ignore_spaces(
    sql,
    "IF XACT_STATE() = 1 COMMIT; IF OBJECT_ID('test.testing', 'U') IS NULL  CREATE TABLE  test.testing  (id int)\nWITH (DISTRIBUTION = REPLICATE);"
  )
})



# Hive tests
