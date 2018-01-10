library("testthat")

test_that("SQL read write", {
  sql1 <- "SELECT * FROM table"
  writeSql(sql1, "test.sql")
  sql2 <- readSql("test.sql")
  file.remove("test.sql")
  expect_equal(sql1, sql2)
})

test_that("renderSqlFile", {
  sql1 <- "SELECT * FROM @table"
  writeSql(sql1, "test1.sql")
  renderSqlFile("test1.sql", "test2.sql", table = "person")
  sql2 <- readSql("test2.sql")
  file.remove("test1.sql")
  file.remove("test2.sql")
  expect_equal(sql2, "SELECT * FROM person")
})

test_that("translateSqlFile", {
  sql1 <- "SELECT DATEADD(dd,1,observation_period_start_date) FROM observation_period"
  writeSql(sql1, "test1.sql")
  translateSqlFile("test1.sql", "test2.sql", targetDialect = "postgresql")
  sql2 <- readSql("test2.sql")
  file.remove("test1.sql")
  file.remove("test2.sql")
  expect_equal(sql2, "SELECT (observation_period_start_date + 1*INTERVAL'1 day') FROM observation_period")
})

test_that("snakeCaseToCamelCase", {
  string1 <- "cdm_database_schema"
  string2 <- snakeCaseToCamelCase(string1)
  expect_equal(string2, "cdmDatabaseSchema")
})

test_that("camelCaseToSnakeCase ", {
  string1 <- "cdmDatabaseSchema"
  string2 <- camelCaseToSnakeCase(string1)
  expect_equal(string2, "cdm_database_schema")
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
  createRWrapperForSql("test.sql", "test.r", "SqlRender", createRoxygenTemplate = TRUE)
  expect_true(file.exists("test.r"))
  unlink("test.r")
})
