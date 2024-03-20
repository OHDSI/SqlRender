# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of SqlRender
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Launch the SqlRender Developer Shiny app
#'
#' @param launch.browser   Should the app be launched in your default browser, or in a Shiny window.
#'                         Note: copying to clipboard will not work in a Shiny window.
#'
#' @details
#' Launches a Shiny app that allows the user to develop SQL and see how it translates to the supported
#' dialects.
#'
#' @export
launchSqlRenderDeveloper <- function(launch.browser = TRUE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertLogical(launch.browser, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  ensure_installed("shinydashboard")
  appDir <- system.file("shinyApps", "SqlDeveloper", package = "SqlRender")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = launch.browser)
}

is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      inform(paste(msg, "Would you like to install it?", sep = "\n"))
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
