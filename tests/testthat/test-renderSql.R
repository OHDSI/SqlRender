library("testthat")

s <- "{DEFAULT @a = '123'} SELECT * FROM table WHERE x = @a AND {@b == 'blaat'}?{y = 1234}:{x = 1};"

test_that("Parameter substitution works", {
  sql <- renderSql(s, a = "abc")$sql
  expect_equal(sql, "SELECT * FROM table WHERE x = abc AND x = 1;")
})

test_that("Empty parameter does not cause trouble", {
  sql <- renderSql(s, a = "abc", b = "")$sql
  expect_equal(sql, "SELECT * FROM table WHERE x = abc AND x = 1;")
})

test_that("Default works", {
  sql <- renderSql(s, b = "1")$sql
  expect_equal(sql, "SELECT * FROM table WHERE x = 123 AND x = 1;")
})

test_that("If-then-else: then works", {
  sql <- renderSql(s, b = "blaat")$sql
  expect_equal(sql, "SELECT * FROM table WHERE x = 123 AND y = 1234;")
})

test_that("If-then-else: else works", {
  sql <- renderSql(s, b = "bla")$sql
  expect_equal(sql, "SELECT * FROM table WHERE x = 123 AND x = 1;")
})

test_that("If-then-else: boolean parameter interpreted as character", {
  sql <- renderSql("SELECT * FROM table {@a}?{WHERE x = 1}", a = FALSE)$sql
  expect_equal(sql, "SELECT * FROM table ")

  sql <- renderSql("SELECT * FROM table {@a}?{WHERE x = 1}", a = TRUE)$sql
  expect_equal(sql, "SELECT * FROM table WHERE x = 1")

})


s <- "{1 IN (@a)}?{SELECT * FROM table}"

test_that("If-then-else: IN pattern works if value is in", {
  sql <- renderSql(s, a = c(1, 2, 3, 4))$sql
  expect_equal(sql, "SELECT * FROM table")
})

test_that("If-then-else: IN pattern works if value is not in", {
  sql <- renderSql(s, a = c(2, 3, 4))$sql
  expect_equal(sql, "")
})

test_that("If-then-else: IN pattern works with space at start", {
  sql <- renderSql("{ 1 IN (@a)}?{SELECT * FROM table}", a = c(2, 3, 4))$sql
  expect_equal(sql, "")
  sql <- renderSql("{ 1 IN (@a)}?{SELECT * FROM table}", a = c(1, 2, 3, 4))$sql
  expect_equal(sql, "SELECT * FROM table")
})

test_that("If-then-else: AND operator", {
  sql <- renderSql("{true & true}?{true}:{false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: AND operator", {
  sql <- renderSql("{true & false}?{true}:{false}")$sql
  expect_equal(sql, "false")
})


test_that("If-then-else: OR operator", {
  sql <- renderSql("{true | false}?{true}:{false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: OR operator", {
  sql <- renderSql("{true | true}?{true}:{false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: OR operator", {
  sql <- renderSql("{false | false}?{true}:{false}")$sql
  expect_equal(sql, "false")
})


test_that("If-then-else: IN pattern nested in boolean condition", {
  sql <- renderSql("{true & (true & (true & 4 IN (@a)))}?{true}:{false}", a = c(1, 2, 3))$sql
  expect_equal(sql, "false")
})


test_that("If-then-else: nested if-then-else where nest in firing expression", {
  sql <- renderSql("{true}?{{true} ? {double true} : {true false}} : {false}")$sql
  expect_equal(sql, "double true")
})

test_that("If-then-else: nested if-then-else where nest in non-firing expression", {
  sql <- renderSql("{false}?{{true} ? {double true} : {true false}} : {false}")$sql
  expect_equal(sql, "false")
})

test_that("If-then-else: simple negation", {
  sql <- renderSql("{!false}?{true}:{false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: negation of parameter", {
  sql <- renderSql("{!@a}?{true}:{false}", a = "true")$sql
  expect_equal(sql, "false")
})

test_that("If-then-else: negation of parameter", {
  sql <- renderSql("{!@a}?{true}:{false}", a = "true")$sql
  expect_equal(sql, "false")
})

test_that("If-then-else: does not equals pattern 1", {
  sql <- renderSql("{123 != 123}?{true}:{false}")$sql
  expect_equal(sql, "false")
})

test_that("If-then-else: does not equals pattern 1", {
  sql <- renderSql("{123 != 234}?{true}:{false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: does not equals pattern 2", {
  sql <- renderSql("{123 <> 123}?{true}:{false}")$sql
  expect_equal(sql, "false")
})

test_that("If-then-else: does not equals pattern 2", {
  sql <- renderSql("{123 <> 234}?{true}:{false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: Nested IN evaluates to true", {
  sql <- renderSql("{TRUE & (FALSE | 1 IN (1,2,3))} ? {true} : {false}")$sql
  expect_equal(sql, "true")
})

test_that("If-then-else: Nested IN evaluates to false", {
  sql <- renderSql("{TRUE & (FALSE | 4 IN (1,2,3))} ? {true} : {false}")$sql
  expect_equal(sql, "false")
})

test_that("Backslash in parameter is handled correctly", {
  sql <- renderSql("SELECT * FROM table WHERE name = '@name';", name = "NA\\joe")$sql
  expect_equal(sql, "SELECT * FROM table WHERE name = 'NA\\joe';")
})


test_that("If-then-else: error on bad boolean logic syntax", {
  # Note there's only one equals sign:
  expect_error(renderSql("{true = true} ? {true} : {false}")$sql)
})

test_that("rendering: warning on parameter name mismatch", {
  # Note wrong parameter name:
  expect_warning(renderSql("SELECT * FROM @my_table", a_table = "x")$sql)
})

test_that("rendering: no problem when not providing parameters", {
  # Note wrong parameter name:
  expect_equal(renderSql("SELECT * FROM @my_table")$sql, "SELECT * FROM @my_table")
})
