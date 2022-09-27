package org.ohdsi.sql;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TestSqlRender {

	public static void main(String[] args) {
		String sql = "WITH cte_all\r\n" + 
				"AS (\r\n" + 
				"	SELECT * FROM @cdmSchema.MEASUREMENT m\r\n" + 
				"		\r\n" + 
				"	UNION ALL\r\n" + 
				"	\r\n" + 
				"	SELECT 	'(Glucose [Mass/volume] in Serum or Plasma --12 hours fasting)' AS check_description\r\n" + 
				"	)\r\n" + 
				"INSERT INTO @resultsSchema.@resultsTable\r\n" + 
				"SELECT *\r\n" + 
				"FROM cte_all;";
		String path = "inst/csv/replacementPatterns.csv";
		sql = SqlTranslate.translateSqlWithPath(sql, "postgresql", null, null, path);
		System.out.println(sql);
		
//		Pattern pattern = Pattern.compile("^((?!FROM).)*$");
//		System.out.println(pattern.matcher("SELECT * blaat b;").matches());
		
//		String sql = "SELECT * FROM table {@a = true} ?  {WHERE name = '@name'};";
//		sql = SqlRender.renderSql(sql, new String[]{"name", "a"}, new String[]{"NA\\joe", "true"});
//		System.out.println(sql);	
//		
//		String sourceSql = "SELECT TOP 10 * FROM my_table WHERE a = b;";
//		String sql;
//		sql = SqlTranslate.translateSqlWithPath(sourceSql, "postgresql", null, null, path);
//		System.out.println(sql);		
//		
//		sql = SqlTranslate.translateSqlWithPath(sourceSql, "oracle", null, null, path);
//		System.out.println(sql);
		
//		String sql = "SELECT * FROM @my_table";
//		for (String warning : SqlRender.checkSql(sql, new String[]{"my_table"}, new String[]{"asdfs"}))
//			System.out.println(warning);

//		String sql = "CREATE TABLE abcdefghijklmnopqrstuvwxyz1234567890;";
//		for (String warning : SqlTranslate.check(sql, ""))
//			System.out.println(warning);
	}
}
