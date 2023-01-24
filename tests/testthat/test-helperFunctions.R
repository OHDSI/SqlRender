library("testthat")

# test_that("Designed to fail", {
#   expect_true(FALSE)
# })

test_that("SQL read write", {
  fileName <- tempfile()
  sql1 <- "SELECT * FROM table"
  writeSql(sql1, fileName)
  sql2 <- readSql(fileName)
  file.remove(fileName)
  expect_equal(sql1, sql2)
})

test_that("renderSqlFile", {
  fileName1 <- tempfile()
  fileName2 <- tempfile()
  sql1 <- "SELECT * FROM @table"
  writeSql(sql1, fileName1)
  renderSqlFile(fileName1, fileName2, table = "person")
  sql2 <- readSql(fileName2)
  file.remove(fileName1)
  file.remove(fileName2)
  expect_equal(sql2, "SELECT * FROM person")
})

test_that("translateSqlFile", {
  fileName1 <- tempfile()
  fileName2 <- tempfile()
  sql1 <- "SELECT DATEADD(dd,1,observation_period_start_date) FROM observation_period"
  writeSql(sql1, fileName1)
  translateSqlFile(fileName1, fileName2, targetDialect = "postgresql")
  sql2 <- readSql(fileName2)
  file.remove(fileName1)
  file.remove(fileName2)
  expect_equivalent(
    sql2,
    "SELECT (observation_period_start_date + 1*INTERVAL'1 day') FROM observation_period"
  )
})

test_that("Warning in translateSqlFile when using oracleTempSchema", {
  fileName1 <- tempfile()
  fileName2 <- tempfile()
  sql1 <- "SELECT DATEADD(dd,1,observation_period_start_date) FROM observation_period"
  writeSql(sql1, fileName1)
  clearWarningBlock()
  expect_warning(translateSqlFile(fileName1, fileName2, targetDialect = "oracle", oracleTempSchema = "scratch"))
  file.remove(fileName1)
  file.remove(fileName2)
})

test_that("snakeCaseToCamelCase", {
  string1 <- "cdm_database_schema"
  string2 <- snakeCaseToCamelCase(string1)
  expect_equal(string2, "cdmDatabaseSchema")

  string1 <- "EXPOSURE_ID_1"
  string2 <- snakeCaseToCamelCase(string1)
  expect_equal(string2, "exposureId1")
})

test_that("camelCaseToSnakeCase ", {
  string1 <- "cdmDatabaseSchema"
  string2 <- camelCaseToSnakeCase(string1)
  expect_equal(string2, "cdm_database_schema")

  string1 <- "exposureId1"
  string2 <- camelCaseToSnakeCase(string1)
  expect_equal(string2, "exposure_id_1")
})

test_that("camelCaseToTitleCase ", {
  string1 <- "cdmDatabaseSchema"
  string2 <- camelCaseToTitleCase(string1)
  expect_equal(string2, "Cdm Database Schema")

  string1 <- "exposureId1"
  string2 <- camelCaseToTitleCase(string1)
  expect_equal(string2, "Exposure Id 1")
})

test_that("loadRenderTranslateSql ", {
  sql <- loadRenderTranslateSql("test.sql", "SqlRender", "sql server")
  sql <- gsub("[\r\n]", "", sql)
  expect_equivalent(sql, "SELECT a FROM #my_table WHERE my_id = 123;")

  sql <- loadRenderTranslateSql("test.sql", "SqlRender", "postgresql")
  sql <- gsub("[\r\n]", "", sql)
  expect_equivalent(sql, "SELECT a FROM my_table WHERE my_id = 123;")

  sql <- loadRenderTranslateSql("test.sql", "SqlRender", "oracle")
  sql <- gsub("[\r\n]", "", sql)
  expect_equivalent(sql, "SELECT a FROM my_table WHERE my_id = 123;")
})

test_that("loadRenderTranslateSql from sql folder", {
  sql <- loadRenderTranslateSql(sqlFilename = "test2.sql", packageName = "SqlRender", dbms = "postgresql")
  sql <- gsub("[\r\n]", "", sql)
  expect_equivalent(sql, "SELECT a FROM my_table WHERE my_id = 123;")
})

test_that("Warning using loadRenderTranslateSql with oracleTempSchema", {
  clearWarningBlock()
  expect_warning(loadRenderTranslateSql(sqlFilename = "test.sql", packageName = "SqlRender", dbms = "oracle", oracleTempSchema = "scratch"))
})

test_that("createRWrapperForSql", {
  fileName <- tempfile()
  createRWrapperForSql(sqlFilename = "test.sql", rFilename = fileName, packageName = "SqlRender", createRoxygenTemplate = TRUE)
  expect_true(file.exists(fileName))
  file.remove(fileName)
})

test_that("listSupportedDialects", {
  dialects <- listSupportedDialects()
  expect_s3_class(dialects, "data.frame")
  expect_true("dialect" %in% colnames(dialects))
})

test_that("snakeCaseToCamelCaseNames", {
  x <- data.frame(concept_id = 1, concept_name = "b")
  x <- snakeCaseToCamelCaseNames(x)
  expect_equal(names(x), c("conceptId", "conceptName"))
})

test_that("camelCaseToSnakeCaseNames", {
  x <- data.frame(conceptId = 1, conceptName = "b")
  x <- camelCaseToSnakeCaseNames(x)
  expect_equal(names(x), c("concept_id", "concept_name"))
})
