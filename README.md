SqlRender
=========

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

Requirements
============
Requires R with the package rJava installed. Also requires Java 1.6 or higher.


Getting Started
===============
Use these commands in R to install the SqlRender package:

```r
install.packages("devtools")
library("devtools")
install_github("ohdsi/SqlRender")
```

Documentation
=============
Please see the [vignette](https://raw.githubusercontent.com/OHDSI/SqlRender/master/vignettes/UsingSqlRender.pdf) for details on how to use SqlRender.

See the [manual](https://raw.githubusercontent.com/OHDSI/SqlRender/master/man/SqlRender.pdf) for a list of all the functions in the package.


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

