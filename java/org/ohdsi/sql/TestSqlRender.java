package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {
//		String sql = "SELECT * FROM table WHERE name = '@name';";
//		sql = SqlRender.renderSql(sql, new String[]{"name"}, new String[]{"NA\\joe"});
//		System.out.println(sql);	
		String path = "inst/csv/replacementPatterns.csv";
		String sourceSql = "SELECT DATEADD(DAY, observation_period_start_date, 1) FROM observation_period;";
		String sql;
		sql = SqlTranslate.translateSqlWithPath(sourceSql, "postgresql", null, null, path);
		System.out.println(sql);		
		
		sql = SqlTranslate.translateSqlWithPath(sourceSql, "oracle", null, null, path);
		System.out.println(sql);

	}
}
