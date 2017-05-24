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
		String sourceSql = "insert into ohdsi.ACHILLES_results (analysis_id, stratum_1, stratum_2, count_value)\n" +
				"select 1802 as analysis_id,   \n" +
				"\tCAST(m.measurement_concept_id AS VARCHAR(255)) as stratum_1,\n" +
				"\tCAST(YEAR(measurement_date)*100 + month(measurement_date) AS VARCHAR(255)) as stratum_2,\n" +
				"\tCOUNT_BIG(distinct PERSON_ID) as count_value\n" +
				"from\n" +
				"\tohdsi.measurement m\n" +
				"group by m.measurement_concept_id, \n" +
				"\tYEAR(measurement_date)*100 + month(measurement_date);";
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
