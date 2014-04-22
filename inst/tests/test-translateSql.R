library("testthat")

test_that("translateSQL sql server -> Oracle DATEDIFF", {
			sql <- translateSql("SELECT DATEDIFF(dd,drug_era_start_date,drug_era_end_date) FROM drug_era;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "SELECT (drug_era_end_date - drug_era_start_date) FROM drug_era;")
		})


test_that("translateSQL sql server -> Oracle DATEADD", {
			sql <- translateSql("SELECT DATEADD(dd,drug_era_end_date,30) FROM drug_era;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "SELECT (drug_era_end_date + 30) FROM drug_era;")
		})

test_that("translateSQL sql server -> Oracle USE", {
			sql <- translateSql("USE vocabulary;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "ALTER SESSION SET current_schema =  vocabulary;")
		})

test_that("translateSQL sql server -> Oracle USE", {
			sql <- translateSql("IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;",sourceDialect = "sql server", targetDialect = "oracle")$sql
			expect_equal(sql, "BEGIN\\n  EXECUTE IMMEDIATE 'TRUNCATE TABLE  cohort;';\\n  EXECUTE IMMEDIATE 'DROP TABLE  cohort;';\\nEXCEPTION\\n  WHEN OTHERS THEN\\n    IF SQLCODE != -942 THEN\\n      RAISE;\\n    END IF;\\nEND;")
		})