package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {

		/*
		String sql = "--HINT DISTRIBUTE_ON_KEY(analysis_id)\nCREATE TABLE results.achilles_results_dist";
		for (String part : SqlSplit.splitSql(sql))
			System.out.println(part);
			*/
		
//		Pattern pattern = Pattern.compile("^((?!FROM).)*$");
//		System.out.println(pattern.matcher("SELECT * blaat b;").matches());
		
//		String sql = "SELECT * FROM table {@a = true} ?  {WHERE name = '@name'};";
//		sql = SqlRender.renderSql(sql, new String[]{"name", "a"}, new String[]{"NA\\joe", "true"});
//		System.out.println(sql);	
//		String path = "inst/csv/replacementPatterns.csv";
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
