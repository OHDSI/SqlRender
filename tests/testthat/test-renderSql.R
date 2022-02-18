library("testthat")

s <- "{DEFAULT @a = '123'} SELECT * FROM table WHERE x = @a AND {@b == 'blaat'}?{y = 1234}:{x = 1};"

test_that("Parameter substitution works", {
  sql <- render(s, a = "abc")
  expect_equal(sql, "SELECT * FROM table WHERE x = abc AND x = 1;")
})

test_that("Empty parameter does not cause trouble", {
  sql <- render(s, a = "abc", b = "")
  expect_equal(sql, "SELECT * FROM table WHERE x = abc AND x = 1;")
})

test_that("Default works", {
  sql <- render(s, b = "1")
  expect_equal(sql, "SELECT * FROM table WHERE x = 123 AND x = 1;")
})

test_that("If-then-else: then works", {
  sql <- render(s, b = "blaat")
  expect_equal(sql, "SELECT * FROM table WHERE x = 123 AND y = 1234;")
})

test_that("If-then-else: else works", {
  sql <- render(s, b = "bla")
  expect_equal(sql, "SELECT * FROM table WHERE x = 123 AND x = 1;")
})

test_that("If-then-else: boolean parameter interpreted as character", {
  sql <- render("SELECT * FROM table {@a}?{WHERE x = 1}", a = FALSE)
  expect_equal(sql, "SELECT * FROM table ")

  sql <- render("SELECT * FROM table {@a}?{WHERE x = 1}", a = TRUE)
  expect_equal(sql, "SELECT * FROM table WHERE x = 1")
})


s <- "{1 IN (@a)}?{SELECT * FROM table}"

test_that("If-then-else: IN pattern works if value is in", {
  sql <- render(s, a = c(1, 2, 3, 4))
  expect_equal(sql, "SELECT * FROM table")
})

test_that("If-then-else: IN pattern works if value is not in", {
  sql <- render(s, a = c(2, 3, 4))
  expect_equal(sql, "")
})

test_that("If-then-else: IN pattern works with space at start", {
  sql <- render("{ 1 IN (@a)}?{SELECT * FROM table}", a = c(2, 3, 4))
  expect_equal(sql, "")
  sql <- render("{ 1 IN (@a)}?{SELECT * FROM table}", a = c(1, 2, 3, 4))
  expect_equal(sql, "SELECT * FROM table")
})

test_that("If-then-else: AND operator", {
  sql <- render("{true & true}?{true}:{false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: AND operator", {
  sql <- render("{true & false}?{true}:{false}")
  expect_equal(sql, "false")
})


test_that("If-then-else: OR operator", {
  sql <- render("{true | false}?{true}:{false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: OR operator", {
  sql <- render("{true | true}?{true}:{false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: OR operator", {
  sql <- render("{false | false}?{true}:{false}")
  expect_equal(sql, "false")
})


test_that("If-then-else: IN pattern nested in boolean condition", {
  sql <- render("{true & (true & (true & 4 IN (@a)))}?{true}:{false}", a = c(1, 2, 3))
  expect_equal(sql, "false")
})


test_that("If-then-else: nested if-then-else where nest in firing expression", {
  sql <- render("{true}?{{true} ? {double true} : {true false}} : {false}")
  expect_equal(sql, "double true")
})

test_that("If-then-else: nested if-then-else where nest in non-firing expression", {
  sql <- render("{false}?{{true} ? {double true} : {true false}} : {false}")
  expect_equal(sql, "false")
})

test_that("If-then-else: simple negation", {
  sql <- render("{!false}?{true}:{false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: negation of parameter", {
  sql <- render("{!@a}?{true}:{false}", a = "true")
  expect_equal(sql, "false")
})

test_that("If-then-else: negation of parameter", {
  sql <- render("{!@a}?{true}:{false}", a = "true")
  expect_equal(sql, "false")
})

test_that("If-then-else: does not equals pattern 1", {
  sql <- render("{123 != 123}?{true}:{false}")
  expect_equal(sql, "false")
})

test_that("If-then-else: does not equals pattern 1", {
  sql <- render("{123 != 234}?{true}:{false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: does not equals pattern 2", {
  sql <- render("{123 <> 123}?{true}:{false}")
  expect_equal(sql, "false")
})

test_that("If-then-else: does not equals pattern 2", {
  sql <- render("{123 <> 234}?{true}:{false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: Nested IN evaluates to true", {
  sql <- render("{TRUE & (FALSE | 1 IN (1,2,3))} ? {true} : {false}")
  expect_equal(sql, "true")
})

test_that("If-then-else: Nested IN evaluates to false", {
  sql <- render("{TRUE & (FALSE | 4 IN (1,2,3))} ? {true} : {false}")
  expect_equal(sql, "false")
})

test_that("Backslash in parameter is handled correctly", {
  sql <- render("SELECT * FROM table WHERE name = '@name';", name = "NA\\joe")
  expect_equal(sql, "SELECT * FROM table WHERE name = 'NA\\joe';")
})

test_that("Dollar in parameter is handled correctly", {
  sql <- render("SELECT * FROM table WHERE name = '@name';", name = "NA$joe")
  expect_equal(sql, "SELECT * FROM table WHERE name = 'NA$joe';")
})

test_that("If-then-else: error on bad boolean logic syntax", {
  # Note there's only one equals sign:
  expect_error(render("{true = true} ? {true} : {false}"))
})

test_that("rendering: warning on parameter name mismatch", {
  # Note wrong parameter name:
  expect_warning(render("SELECT * FROM @my_table", a_table = "x"))
})

test_that("rendering: no problem when not providing parameters", {
  # Note wrong parameter name:
  expect_equal(render("SELECT * FROM @my_table"), "SELECT * FROM @my_table")
})

test_that("rendering: warning when using old function", {
  expect_warning(renderSql("SELECT * FROM @my_table"))
})
