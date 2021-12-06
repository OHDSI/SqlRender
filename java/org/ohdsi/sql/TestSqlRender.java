package org.ohdsi.sql;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TestSqlRender {

	public static void main(String[] args) {
		
		String regex = "([a-z]+\\(.*\\))|([a-z0-9_]+)";
		Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE | Pattern.DOTALL | Pattern.MULTILINE);
		String string = "SELECT CAST(middle_initial AS VARCHAR)";
		Matcher matcher = pattern.matcher(string);
		while (matcher.find())
			if (matcher.end() == string.length())
				System.out.println("check");
	
		String sql = "SELECT CAST(middle_initial AS VARCHAR) + 'abc' FROM my_table;";
		String path = "inst/csv/replacementPatterns.csv";
		sql = SqlTranslate.translateSqlWithPath(sql, "test", null, null, path);
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
