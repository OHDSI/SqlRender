library("testthat")

s <- "{DEFAULT @a = '123'} SELECT * FROM table WHERE x = @a AND {@b == 'blaat'}?{y = 1234}:{x = 1};"

test_that("Parameter substitution works", {
			sql <- renderSql(s,a="abc")$sql
			expect_equal(sql, " SELECT * FROM table WHERE x = abc AND x = 1;")
		})

test_that("Empty parameter does not cause trouble", {
			sql <- renderSql(s,a="abc",b="")$sql
			expect_equal(sql, " SELECT * FROM table WHERE x = abc AND x = 1;")
		})

test_that("Default works", {
			sql <- renderSql(s,b="1")$sql
			expect_equal(sql, " SELECT * FROM table WHERE x = 123 AND x = 1;")
		})

test_that("If-then-else: then works", {
			sql <- renderSql(s,b="blaat")$sql
			expect_equal(sql, " SELECT * FROM table WHERE x = 123 AND y = 1234;")
		})

test_that("If-then-else: else works", {
			sql <- renderSql(s,b="bla")$sql
			expect_equal(sql, " SELECT * FROM table WHERE x = 123 AND x = 1;")
		})


s <- "{1 IN (@a)}?{SELECT * FROM table}"

test_that("If-then-else: IN pattern works if value is in", {
			sql <- renderSql(s,a = c(1,2,3,4))$sql
			expect_equal(sql, "SELECT * FROM table")
		})

test_that("If-then-else: IN pattern works if value is not in", {
			sql <- renderSql(s,a = c(2,3,4))$sql
			expect_equal(sql, "")
		})

test_that("If-then-else: IN pattern nested in boolean condition", {
			sql <- renderSql("{true & (true & (true & 4 IN (@a)))}?{true}:{false}",a = c(1,2,3))$sql
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