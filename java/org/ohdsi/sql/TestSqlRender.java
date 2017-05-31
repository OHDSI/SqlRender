package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {
//		Pattern pattern = Pattern.compile("^((?!FROM).)*$");
//		System.out.println(pattern.matcher("SELECT * blaat b;").matches());
		
//		String sql = "SELECT * FROM table {@a = true} ?  {WHERE name = '@name'};";
//		sql = SqlRender.renderSql(sql, new String[]{"name", "a"}, new String[]{"NA\\joe", "true"});
//		System.out.println(sql);	
		String path = "inst/csv/replacementPatterns.csv";
		//String sourceSql = "SELECT TOP 10 * FROM my_table WHERE a = b;";
		String sourceSql = "insert into ohdsi.ACHILLES_results_derived (stratum_1,statistic_value,measure_id)    \n" +
				"select decade as stratum_1,temp_cnt as statistic_value,\n" +
				"'Death:byDecade:SafePatientCnt' as measure_id\n" +
				"from\n" +
				"   (select left(stratum_1,3) as decade,sum(count_value) as temp_cnt from  ohdsi.achilles_results where analysis_id = 504  group by left(stratum_1,3)\n" +
				"   )a\n" +
				"where temp_cnt >= 11;";
		String sql;
		sql = SqlTranslate.translateSqlWithPath(sourceSql, "bigquery", null, null, path);
		System.out.println(sql);		
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
