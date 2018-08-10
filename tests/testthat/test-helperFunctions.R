library("testthat")

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
  expect_equal(sql2, "SELECT (observation_period_start_date + 1*INTERVAL'1 day') FROM observation_period")
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

test_that("loadRenderTranslateSql ", {
  sql <- loadRenderTranslateSql("test.sql", "SqlRender", "sql server")
  sql <- gsub("[\r\n]", "", sql)
  expect_equal(sql, "SELECT * FROM table;")

  sql <- loadRenderTranslateSql("test.sql", "SqlRender", "postgresql")
  sql <- gsub("[\r\n]", "", sql)
  expect_equal(sql, "SELECT * FROM table;")

  sql <- loadRenderTranslateSql("test.sql", "SqlRender", "oracle")
  sql <- gsub("[\r\n]", "", sql)
  expect_equal(sql, "SELECT a FROM table;")
})

test_that("createRWrapperForSql", {
  fileName <- tempfile()
  createRWrapperForSql("test.sql", fileName, "SqlRender", createRoxygenTemplate = TRUE)
  expect_true(file.exists(fileName))
  file.remove(fileName)
})
