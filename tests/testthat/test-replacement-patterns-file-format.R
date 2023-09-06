test_that("replacementPatterns.csv has valid format", {
  lines <- readLines(system.file("csv", "replacementPatterns.csv", package = "SqlRender"))
  for (line in lines) {
    lineTrim <- gsub('"[^"]*"', "", gsub('\\\\"', "", line))
    comma_cnt <- nchar(lineTrim) - nchar(gsub(",", "", lineTrim))
    expect_equal(comma_cnt, 2, label = paste0("comma counts in string: ", line))
  }
})
