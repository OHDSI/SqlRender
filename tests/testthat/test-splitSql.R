library("testthat")

test_that("splitSql split simple statements", {
  sql <- splitSql("SELECT * INTO a FROM b; USE x; DROP TABLE c;")
  expect_equal(sql, c("SELECT * INTO a FROM b", "USE x", "DROP TABLE c"))
})

test_that("splitSql split with BEGIN...END", {
  sql <- splitSql("BEGIN\nSELECT * INTO a FROM b;\nEND;\nUSE x;")
  expect_equal(sql, c("BEGIN\nSELECT * INTO a FROM b;\nEND;", "USE x"))
})

test_that("splitSql split with CASE...END", {
  sql <- splitSql("SELECT CASE WHEN x=1 THEN 0 ELSE 1 END AS x INTO a FROM b;\nUSE x;")
  expect_equal(sql, c("SELECT CASE WHEN x=1 THEN 0 ELSE 1 END AS x INTO a FROM b", "USE x"))
})


test_that("splitSql split with 'end' in quoted text", {
  sql <- splitSql("insert into a (x) values ('end');\n insert into a (x) values ('begin');")
  expect_equal(sql, c("insert into a (x) values ('end')", "insert into a (x) values ('begin')"))
})

test_that("splitSql split with case end at the end", {
  sql <- splitSql("SELECT CASE WHEN x=1 THEN 0 ELSE 1 END FROM a GROUP BY CASE WHEN x=1 THEN 0 ELSE 1 END;")
  expect_equal(sql,
               c("SELECT CASE WHEN x=1 THEN 0 ELSE 1 END FROM a GROUP BY CASE WHEN x=1 THEN 0 ELSE 1 END"))
})


test_that("splitSql split with reserved word 'end' as field name", {
  sql <- splitSql("INSERT INTO CDM_CPRD_TESTING_RAW.dbo.hes_linkage_coverage (data_source, start, [end]) VALUES ('hes', '1990-01-01', '2014-12-31');")
  expect_equal(sql,
               c("INSERT INTO CDM_CPRD_TESTING_RAW.dbo.hes_linkage_coverage (data_source, start, [end]) VALUES ('hes', '1990-01-01', '2014-12-31')"))
})

test_that("splitSql split with last line containing comment and having no EOL", {
  sql <- splitSql("SELECT * FROM table;\n-- end")
  expect_equal(sql,
               c("SELECT * FROM table"))
})
