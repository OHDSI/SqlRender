library("testthat")

s <- "{DEFAULT @a = '123'} SELECT * FROM table WHERE x = @a AND {@b == 'blaat'}?{y = 1234}:{x = 1};"

test_that("Parameter substitution works", {
  sql <- renderSql(s,a="abc")$sql
  expect_equal(sql, " SELECT * FROM table WHERE x = abc AND x = 1;")
})

test_that("If-then-else: then works", {
  sql <- renderSql(s,b="blaat")$sql
  expect_equal(sql, " SELECT * FROM table WHERE x = 123 AND y = 1234;")
})

test_that("If-then-else: else works", {
  sql <- renderSql(s,b="bla")$sql
  expect_equal(sql, " SELECT * FROM table WHERE x = 123 AND x = 1;")
})
