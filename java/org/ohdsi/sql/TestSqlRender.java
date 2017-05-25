package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {
//		Pattern pattern = Pattern.compile("^((?!FROM).)*$");
//		System.out.println(pattern.matcher("SELECT * blaat b;").matches());
		
//		String sql = "SELECT * FROM table {@a = true} ?  {WHERE name = '@name'};";
//		sql = SqlRender.renderSql(sql, new String[]{"name", "a"}, new String[]{"NA\\joe", "true"});
//		System.out.println(sql);	
		//String path = "/Users/myl/mylSqlRender/SqlRender/inst/csv/replacementPatterns.csv";
		String path = "inst/csv/replacementPatterns.csv";
		//String sourceSql = "SELECT TOP 10 * FROM my_table WHERE a = b;";
		String sourceSql = "select 101 as analysis_id,   CAST(year(op1.index_date) - p1.YEAR_OF_BIRTH AS VARCHAR(255)) as stratum_1, COUNT_BIG(p1.person_id) as count_value\n" +
				"from ohdsi.PERSON p1\n" +
				"        inner join (select person_id, MIN(observation_period_start_date) as index_date from ohdsi.OBSERVATION_PERIOD group by PERSON_ID) op1\n" +
				"        on p1.PERSON_ID = op1.PERSON_ID\n" +
				"group by year(op1.index_date) - p1.YEAR_OF_BIRTH;";
		String sql;
		sql = SqlTranslate.translateSqlWithPath(sourceSql, "bigquery", null, null, path);
		System.out.println(sql.toLowerCase());
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
