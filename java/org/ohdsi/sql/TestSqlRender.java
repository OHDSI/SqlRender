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
		String sourceSql = "with rawData(person_id, count_value) as\n" +
				"(\n" +
				"  select person_id, COUNT_BIG(distinct condition_concept_id) as num_conditions\n" +
				"  from ohdsi.condition_occurrence\n" +
				"\tgroup by person_id\n" +
				"),\n" +
				"overallStats (avg_value, stdev_value, min_value, max_value, total) as\n" +
				"(\n" +
				"  select CAST(avg(1.0 * count_value) AS FLOAT) as avg_value,\n" +
				"    CAST(stdev(count_value) AS FLOAT) as stdev_value,\n" +
				"    min(count_value) as min_value,\n" +
				"    max(count_value) as max_value,\n" +
				"    count_big(*) as total\n" +
				"  from rawData\n" +
				"),\n" +
				"statsView (count_value, total, rn) as\n" +
				"(\n" +
				"  select count_value, \n" +
				"  \tcount_big(*) as total, \n" +
				"\t\trow_number() over (order by count_value) as rn\n" +
				"  FROM rawData\n" +
				"  group by count_value\n" +
				"),\n" +
				"priorStats (count_value, total, accumulated) as\n" +
				"(\n" +
				"  select s.count_value, s.total, sum(p.total) as accumulated\n" +
				"  from statsView s\n" +
				"  join statsView p on p.rn <= s.rn\n" +
				"  group by s.count_value, s.total, s.rn\n" +
				")\n" +
				"select 403 as analysis_id,\n" +
				"  o.total as count_value,\n" +
				"  o.min_value,\n" +
				"\to.max_value,\n" +
				"\to.avg_value,\n" +
				"\to.stdev_value,\n" +
				"\tMIN(case when p.accumulated >= .50 * o.total then count_value else o.max_value end) as median_value,\n" +
				"\tMIN(case when p.accumulated >= .10 * o.total then count_value else o.max_value end) as p10_value,\n" +
				"\tMIN(case when p.accumulated >= .25 * o.total then count_value else o.max_value end) as p25_value,\n" +
				"\tMIN(case when p.accumulated >= .75 * o.total then count_value else o.max_value end) as p75_value,\n" +
				"\tMIN(case when p.accumulated >= .90 * o.total then count_value else o.max_value end) as p90_value\n" +
				"into #tempResults\n" +
				"from priorStats p\n" +
				"CROSS JOIN overallStats o\n" +
				"GROUP BY o.total, o.min_value, o.max_value, o.avg_value, o.stdev_value";
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
