test_that("replacementPatterns.csv has valid format", {
  lines <- readLines(system.file('csv', 'replacementPatterns.csv', package = 'SqlRender'))
  for (line in lines) {
    line_trim <- stringr::str_replace_all(line, '".[^"]+"', '')
    comma_cnt <-  stringr::str_count(line_trim, ',')
    expect_equal(comma_cnt, 2, label = paste0('comma counts in string: ', line))
  }
})
