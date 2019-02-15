SqlRender 1.6.0
===============

Changes:

1. Major overhaul of BigQuery translation.

2. Added support for SQLite.

3. ISNUMERIC translation implemented for Impala, Netezza, and BigQuery.

4. Performance improvement for Impala temp tables (CREATE TABLE ... STORED AS PARQUET).

5. Adding functions `render` and `translate` that output strings instead of lists. Deprecating `renderSql` and `translateSql`.

6. Added function `translateSingleStatement`.

Bugfixes:

1. Dropping WHERE clause when translating CREATE INDEX for PDW.

2. Fixed PDW's equivalent of CREATE TABLE IF NOT EXISTS.


SqlRender 1.5.3
===============

Changes:

1. Added translation rules for DATETIME2.

Bugfixes:

1. Fixed misspelling of DISTRIBUTE in Netezza translation rules.

SqlRender 1.5.2
===============

Changes:

1. Translation for indexes for RedShift and Impala.
2. Translation for UPDATE STATISTICS.

Bugfixes:

1. Fixed translation of AS when used for table names in Oracle.

SqlRender 1.5.0
================

Changes:

1. Improved support for Netezza.
2. Added random distribution hints for PDW, RedShift, Netezza.
3. Improved MPP index translation for PDW and Netezza.
4. Warnings about missing parameters when rendering SQL can now be turned off.

Bugfixes:

1. Translation of camelCase to snake_case now adds underscore before numbers.

SqlRender 1.4.8
================

Changes: 

1. Better handling of 'FROM DUAL' for Oracle.
2. Improved support for Netezza and Impala.

SqlRender 1.4.6
================

Changes: 

1. Bigquery support for mismatched string and int arguments in coalesce.
2. Translate decimal to float for BigQuery.
3. Created rules to add dummy 'group by' for Oracle statements combining 'case' and 'count' to prevent Oracle from crashing.
4. Adding 'UNBOUNDED PRECEDING' to RedShift windowing functions.


SqlRender 1.4.3
================

Changes: 

1. Added a Shiny app for developing parameterized SQL, and view how this would be rendered and translated into the various supported dialects.
2. Added support for Google BigQuery.
3. Added many more rules for Amazon RedShift, including support for optimization hints.
4. Added rules for DELETE FROM translation for Impala.

Bugfixes:

1. Fixed issue when splitting SQL containing hints.


SqlRender 1.3.7
================

Changes: 

1. Added translation rules for ISNUMERIC and LOG(@expression, @base)


Bugfixes:

1. Fixed bug when trying to split SQL where reserved word 'end' is used as a field name.
2. Fixes for Impala translations.
3. Fixed translation issues for Oracle involving 'FROM DUAL'.
4. Added workaround for Oracle bug for intervals greater than 99 days.
5. Fixed bug when trying to split SQL where last line has comment but no EOL.


SqlRender 1.3.0
================

Changes: 

1. Added ability to use regular expression in translation patterns. This allowed SELECT TOP n to be translated.
2. Deprecated sourceDialect argument.
3. Added translation for CONCAT function with >2 arguments to Oracle (which only allows 2 arguments)
4. Added hints for translation optimation to massive parallel platforms like RedShift
5. Throw warnings when translateSql is called with variable names that are not in the SQL
6. Throw warnings when table names are too long for Oracle


Bugfixes:

1. Fixed translation for date functions so they will now work properly with datetime fields as well.
2. Now throwing error when boolean logic cannot be parsed (instead of assuming result is TRUE)


SqlRender 1.2.0
================

Changes: 

1. Added support for Impala

Bugfixes:

1. Fixed translation for DATEFROMPARTS for RedShift


SqlRender 1.1.7
================

Changes: initial submission to CRAN
