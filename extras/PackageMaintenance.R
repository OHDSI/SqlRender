# @file PackageMaintenance
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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

# Format and check codeP
styler::style_pkg()
OhdsiRTools::checkUsagePackage("SqlRender")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual and vignettes:
unlink("extras/SqlRender.pdf")
shell("R CMD Rd2pdf ./ --output=extras/SqlRender.pdf")

dir.create("inst/doc")
rmarkdown::render("vignettes/UsingSqlRender.Rmd",
                  output_file = "../inst/doc/UsingSqlRender.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/UsingSqlRender.tex")

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Store JAR checksum --------------------------------------------------------------
checksum <- rJava::J("org.ohdsi.sql.JarChecksum", "computeJarChecksum")
write(checksum, file.path("inst", "csv", "jarChecksum.txt"))


# Release package:
devtools::check_win_devel()

devtools::check_rhub()

devtools::release()
