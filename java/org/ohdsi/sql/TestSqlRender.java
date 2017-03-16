package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {
//		String sql = "SELECT * FROM table WHERE name = '@name';";
//		sql = SqlRender.renderSql(sql, new String[]{"name"}, new String[]{"NA\\joe"});
//		System.out.println(sql);	
		String path = "inst/csv/replacementPatterns.csv";
		String sourceSql = "SELECT TOP 10 * FROM my_table WHERE a = b;";
		String sql;
		sql = SqlTranslate.translateSql(sourceSql, "sql server", "postgresql", null, null, path);
		System.out.println(sql);		
		
		sql = SqlTranslate.translateSql(sourceSql, "sql server", "oracle", null, null, path);
		System.out.println(sql);

	}
}
