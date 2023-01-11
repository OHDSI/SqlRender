SqlRender
=========

[![Build Status](https://github.com/OHDSI/SqlRender/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/SqlRender/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/SqlRender/coverage.svg?branch=main)](https://app.codecov.io/github/OHDSI/SqlRender)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/SqlRender)](https://cran.r-project.org/package=SqlRender)
[![CRAN_Status_Badge](http://cranlogs.r-pkg.org/badges/SqlRender)](https://cran.r-project.org/package=SqlRender)

SqlRender is part of [HADES](https://ohdsi.github.io/Hades/).

Introduction
============
This is an R package for rendering parameterized SQL, and translating it to different SQL dialects. SqlRender can also be used as a stand-alone Java library and a command-line executable.

Features
========
- Supports a simple markup syntax for making SQL parameterized, and renders parameterized SQL (containing the markup syntax) to executable SQL
- The syntax supports defining default parameter values
- The syntax supports if-then-else structures
- Has functions for translating SQL from one dialect (Microsoft SQL Server) to other dialects (Oracle, PostgreSQL, Amazon RedShift, Impala, IBM Netezza, Google BigQuery, Microsoft PDW, Snowflake, Azure Synapse, Apache Spark and SQLite)
- Can be used as R package, Java library, or as stand-alone executable through a command-line interface

Examples
========
This example shows the use of parameters, as well as SqlRender's {if} ? {then} : {else} syntax:

```r
sql <- render("SELECT * FROM @a; {@b != ''}?{USE @b;}", a = "my_table", b = "my_schema")
```

will produce the variable `sql` containing this value: 

```
"SELECT * FROM my_table; USE my_schema;"
```

subsequently running this code

```r
sql <- translate(sql, "oracle")
```

will produce the variable `sql` containing this value: 

```
"SELECT * FROM my_table; ALTER SESSION SET current_schema =  my_schema;"
```

## SqlDeveloper

The SqlDeveloper Shiny app is included in the SqlRender R package, and allows viewing the rendering and translation on the fly as you develop your SQL. The SqlDeveloper app is also available online [here](https://data.ohdsi.org/SqlDeveloper/).

Technology
==========
The SqlRender package is an R package wrapped around a Java library. The rJava package is used as interface.

The Java library is available as a JAR file.

System Requirements
===================
Running the package requires R with the package rJava installed. Also requires Java 1.6 or higher.
It is highly recommended for best performance to use a version of R that is at least version 4 or higher otherwise, unexpected bugs or errors may occur.

Installation
=============
## R package

In R, to install the latest stable version, install from CRAN:

```r
install.packages("SqlRender")
```
  
To install the latest development version, install from GitHub:

```r
install.packages("remotes")
remotes::install_github("ohdsi/SqlRender", ref = "develop")
```

Once installed, you can try out SqlRender in a Shiny app that comes with the package:

```r
library(SqlRender)
launchSqlRenderDeveloper()
```

## Java library
You can fetch the JAR file in the inst/java folder of this repository, or use Maven:

1. First add the SqlRender repository so that maven can find and download the SqlRender artifact automatically:
```xml
<repository>
  <id>ohdsi</id>
  <name>repo.ohdsi.org</name>
  <url>https://repo.ohdsi.org/nexus/content/repositories/releases</url>
</repository>
<repository>
  <id>ohdsi.snapshots</id>
  <name>repo.ohdsi.org-snapshots</name>
  <url>https://repo.ohdsi.org/nexus/content/repositories/snapshots</url>
  <releases>
      <enabled>false</enabled>
  </releases>
  <snapshots>
      <enabled>true</enabled>
  </snapshots>
</repository>
```
2: Include the SqlRender dependency in your pom.xml
```xml
<dependency>
	<groupId>org.ohdsi.sql</groupId>
	<artifactId>SqlRender</artifactId>
	<version>1.9.2-SNAPSHOT</version>
</dependency>
```

## Command-line executable
You can fetch the JAR file in the inst/java folder of this repository, or use Maven as described above. Run this from the command line to get a list of options:
```
java -jar SqlRender.jar ?
```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/SqlRender/).

PDF versions of the documentation are also available:
* Vignette: [Using SqlRender](https://github.com/OHDSI/SqlRender/raw/main/inst/doc/UsingSqlRender.pdf)
* Package manual: [SqlRender manual](https://github.com/OHDSI/SqlRender/raw/main/extras/SqlRender.pdf) 

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/SqlRender/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
SqlRender is licensed under Apache License 2.0

Development
===========
SqlRender is being developed in R Studio.

### Development status

Stable. The code is actively being used in several projects.

Acknowledgements
================
- This project is supported in part through the National Science Foundation grant IIS 1251151.

