library("testthat")

# TrexDB runs DuckDB, so the trexdb dialect is an exact copy of the duckdb one.

test_that("trexdb replacement patterns are identical to duckdb's", {
  lines <- readLines(system.file("csv", "replacementPatterns.csv", package = "SqlRender"))
  trexdb <- sub("^trexdb,", "", lines[startsWith(lines, "trexdb,")])
  duckdb <- sub("^duckdb,", "", lines[startsWith(lines, "duckdb,")])
  expect_gt(length(trexdb), 0)
  expect_identical(trexdb, duckdb)
})

test_that("trexdb is a supported dialect", {
  expect_true("trexdb" %in% listSupportedDialects()$dialect)
})

test_that("translate sql server -> TrexDB matches DuckDB", {
  sql <- "SELECT TOP 10 DATEADD(day, 1, a), ISNULL(b, 0), 'x' + c FROM #t WHERE DATEDIFF(day, d, e) > 0;"
  expect_equal(
    as.character(translate(sql, targetDialect = "trexdb")),
    as.character(translate(sql, targetDialect = "duckdb"))
  )
})
