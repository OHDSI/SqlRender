SqlRender
=========

Introduction
============
This is an R package for rendering parameterized SQL, and translating it to different SQL dialects. 

Features
========
- Supports a simple markup syntax for making SQL parameterized, and renders parameterized SQL (containing the markup syntax) to executable SQL
- The syntax supports defining default parameter values
- The syntax supports if-then-else structures
- Has functions for translating SQL from one dialect (Microsoft SQL Server) to other dialects (Oracle, PostgreSQL, Amazon RedShift)
- The Java library used in this package can also be used as a stand-alone library in Java applications.

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
SqlRender is an R package wrapped around a Java library. The rJava package is used as interface. 

System Requirements
===================
Requires R with the package rJava installed. Also requires Java 1.6 or higher.

Dependencies
============
 * There are no dependencies.

Getting Started
===============
Use these commands in R to download and install the SqlRender package:

```r
install.packages("devtools")
library("devtools")
install_github("ohdsi/SqlRender")
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

Stable. The code is actively being used in several projects.

Acknowledgements
================
- This project is supported in part through the National Science Foundation grant IIS 1251151.

