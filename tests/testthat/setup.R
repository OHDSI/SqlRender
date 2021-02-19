clearWarningBlock <- function() {
  if (exists("oracleTempSchema", envir = rlang:::warning_freq_env)) {
    remove("oracleTempSchema", envir = rlang:::warning_freq_env)
  }
}
