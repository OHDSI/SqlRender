SqlRender
=========

This is an R package for rendering parameterized SQL, and translating it to different SQL dialects. For example:
```r
sql <- renderSql("SELECT * FROM @a; {@b != ''}?{USE @b;}", a = "my_table", b = "my_schema")$sql
sql <- translateSql(sql, "sql server", "oracle")$sql
splitSql(sql)
```
will produce a vector with these two values:
```
"SELECT * FROM my_table" 
"ALTER SESSION SET current_schema =  my_schema"
```

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.

