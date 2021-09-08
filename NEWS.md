SqlRender 1.8.1
===============

1. Provide informative error message when Java is outdated.


SqlRender 1.8.0
===============

Changes:

1. Added Apache Spark dialect ("spark").

2. Adding automated check whether correct Java Jar file is loaded, throws warning if not.

3. Adding translation of `CEILING()` for SQLite.


Bugfixes:

1. Fixing setting of global `tempEmulationSchema` option.

2. Workaround for `SUBSTR()` function bug in newer versions of SQLite (by explicitly casting string to type `STRING`).


SqlRender 1.7.0
===============

Changes:

1. Deprecating `oracleTempSchema` argument in various functions in favor of `tempEmulationSchema` schema, which can also be set globally using the `sqlRenderTempEmulationSchema` option.

2. Adding translation of DATEDIFF(YEAR, ...) and DATEDIFF(MONTH, ...) to all dialects.

3. Updated `createRWrapperForSql()` to latest SqlRender standards.


Bugfixes:

1. Fixed translation of CTE without FROM or UNION in BigQuery. 

2. Fixed translation of CONVERT(DATE...) in SQLite.

3. Fixed translation of DATEDIFF with literals in SQLite.


SqlRender 1.6.8
===============

Bugfixes:

1. Fixing error when SQL is not a native character vector (e.g. when it has been created using glue).


SqlRender 1.6.7
===============

Changes:

1. Throw a more informative error message when `loadRenderTranslateSql()` cannot find the SQL file in the specified package.

Bugfixes:

1. On SQLite, DATEADD and CONVERT functions now cast to REAL (used to represent DATE / DATETIME). 

2. On SQLite, DATEADD function now works when amount to add is not a verbatim number.


SqlRender 1.6.6
===============

Changes:

1. Adding rules for modulus operator for BigQuery.

2. Deleting UPDATE STATISTICS statement for BigQuery.


SqlRender 1.6.5
===============

Bugfixes:

1. Fixed CAST(@a as DATE) for YYYYMMDD string dates on BigQuery.


SqlRender 1.6.4
===============

Changes:

1. Adding support for Apache Hive LLAP.

2. Adding functions to convert camelCase to Title Case. (camelCaseToTitleCase)


SqlRender 1.6.3
===============

Changes:

1. Added rules for SQLite for LEFT and RIGHT functions.

2. SQLite now dropping schema name when creating and dropping index (as this throws an error if left).

3.  No longer automatically casting literal to TEXT in RedShift CTE. Users are required to do explicit casts instead.

4. BigQuery insertTable now also uses CTAS hack.

5. Added translation rules for HASHBYTES.

Bugfixes:

1. Fixing GETDATE translation for SQLite.

2. When calling 'render', the replacement value can now contain a $ sign. (Previously this caused an error).

3. isNumeric can now also be applied to numeric fields in Postgres.

4. Better handling of illegal characters in Impala.


SqlRender 1.6.2
===============

Changes:

1. Added rules for Oracle for conditional indices.

Bugfixes:

1. Fixing erroneous variable name translation for BigQuery.


SqlRender 1.6.1
===============

Changes:

1. Added rules for Impala for INTEGER NOT NULL and DOUBLE PRECISION.

Bugfixes:

1. Fixed isNumeric check for Netezza


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

4. Added hints for translation optimization to massive parallel platforms like RedShift

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
