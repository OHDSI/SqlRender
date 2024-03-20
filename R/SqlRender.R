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

#' @keywords internal
"_PACKAGE"

#' @importFrom utils install.packages menu read.csv
#' @importFrom rlang abort warn inform
NULL

.onLoad <- function(libname, pkgname) {
  rJava::.jpackage(pkgname, lib.loc = libname)

  # Verify checksum of JAR:
  storedChecksum <- scan(
    file = system.file("csv", "jarChecksum.txt", package = "SqlRender"),
    what = character(), quiet = TRUE
  )
  computedChecksum <- tryCatch(rJava::J("org.ohdsi.sql.JarChecksum", "computeJarChecksum"),
    error = function(e) {
      warning("Problem connecting to Java. This is normal when runing roxygen.")
      return("")
    }
  )
  if (computedChecksum != "" && (storedChecksum != computedChecksum)) {
    warning("Java library version does not match R package version! Please try reinstalling the SqlRender package.
            Make sure to close all instances of R, and open only one instance before reinstalling. Also make sure your
            R workspace is not reloaded on startup. Delete your .Rdata file if necessary")
  }
}
