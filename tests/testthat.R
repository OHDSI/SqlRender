if (SqlRender::supportsJava8()) {
  library(testthat)
  test_check("SqlRender")
}
