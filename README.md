SqlRender
=========

Introduction
============
This is an R package for rendering parameterized SQL, and translating it to different SQL dialects. SqlRender can also be used as a stand-alone Java library.

Features
========
- Supports a simple markup syntax for making SQL parameterized, and renders parameterized SQL (containing the markup syntax) to executable SQL
- The syntax supports defining default parameter values
- The syntax supports if-then-else structures
- Has functions for translating SQL from one dialect (Microsoft SQL Server) to other dialects (Oracle, PostgreSQL, Amazon RedShift)

Examples
========
This exampe shows the use of parameters, as well as SqlRender's {if} ? {then} : {else} syntax:

```r
sql <- renderSql("SELECT * FROM @a; {@b != ''}?{USE @b;}", a = "my_table", b = "my_schema")$sql
```

will produce the variable `sql` containing this value: 

```
"SELECT * FROM my_table; USE my_schema;"
```

subsequently running this code

```r
sql <- translateSql(sql, "sql server", "oracle")$sql
```

will produce the variable `sql` containing this value: 

```
"SELECT * FROM my_table; ALTER SESSION SET current_schema =  my_schema;"
```

Technology
============
The SqlRender package is an R package wrapped around a Java library. The rJava package is used as interface.

The Java library is available as a JAR file.

System Requirements
===================
Running the package requires R with the package rJava installed. Also requires Java 1.6 or higher.

Dependencies
============
 * There are no dependencies.

Getting Started
===============
## R package

Use these commands in R to download and install the SqlRender package:

```r
install.packages("devtools")
library("devtools")
install_github("ohdsi/SqlRender")
```

## Java library
You can fetch the JAR file in the inst/java folder of this repository, or use Maven:

1. First add the SqlRender repository so that maven can find and download the SqlRender artifact automatically:
```xml
<repositories>
	<repository>
		<id>ohdsi</id>
		<name>repo.ohdsi.org</name>
		<url>http://repo.ohdsi.org:8085/nexus/content/repositories/releases</url>
	</repository>
	<repository>
		<id>ohdsi.snapshots</id>
		<name>repo.ohdsi.org-snapshots</name>
		<url>http://repo.ohdsi.org:8085/nexus/content/repositories/snapshots</url>
		<releases>
			<enabled>false</enabled>
		</releases>
		<snapshots>
			<enabled>true</enabled>
		</snapshots>
	</repository>
</repositories>
```
2: Include the SqlRender dependency in your pom.xml
```xml
<dependency>
	<groupId>org.ohdsi.sql</groupId>
	<artifactId>SqlRender</artifactId>
	<version>1.0.0-SNAPSHOT</version>
</dependency>
```

Getting Involved
=============
* Vignette: [Using SqlRender](https://raw.githubusercontent.com/OHDSI/SqlRender/master/vignettes/UsingSqlRender.pdf)
* Package manual: [SqlRender manual](https://raw.githubusercontent.com/OHDSI/SqlRender/master/man/SqlRender.pdf) 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
SqlRender is licensed under Apache License 2.0

Development
===========
SqlRender is being developed in R Studio.

###Development status
[![Build Status](https://travis-ci.org/OHDSI/SqlRender.svg?branch=master)](https://travis-ci.org/OHDSI/SqlRender)
[![codecov.io](https://codecov.io/github/OHDSI/SqlRender/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/SqlRender?branch=master)

Stable. The code is actively being used in several projects.

Acknowledgements
================
- This project is supported in part through the National Science Foundation grant IIS 1251151.

