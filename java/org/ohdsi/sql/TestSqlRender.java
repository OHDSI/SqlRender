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
		String sourceSql = "with rawData (person_id, age_value) as\n" +
				"(\n" +
				"select p.person_id,\n" +
				"  MIN(YEAR(observation_period_start_date)) - P.YEAR_OF_BIRTH as age_value\n" +
				"  from ohdsi.PERSON p\n" +
				"  JOIN ohdsi.OBSERVATION_PERIOD op on p.person_id = op.person_id\n" +
				"  group by p.person_id, p.year_of_birth\n" +
				"),\n" +
				"overallStats (avg_value, stdev_value, min_value, max_value, total) as\n" +
				"(\n" +
				"  select CAST(avg(1.0 * age_value) AS FLOAT) as avg_value,\n" +
				"  CAST(stdev(age_value) AS FLOAT) as stdev_value,\n" +
				"  min(age_value) as min_value,\n" +
				"  max(age_value) as max_value,\n" +
				"  count_big(*) as total\n" +
				"  FROM rawData\n" +
				"),\n" +
				"ageStats (age_value, total, rn) as\n" +
				"(\n" +
				"  select age_value, count_big(*) as total, row_number() over (order by age_value) as rn\n" +
				"  from rawData\n" +
				"  group by age_value\n" +
				"),\n" +
				"ageStatsPrior (age_value, total, accumulated) as\n" +
				"(\n" +
				"  select s.age_value, s.total, sum(p.total) as accumulated\n" +
				"  from ageStats s\n" +
				"  join ageStats p on p.rn <= s.rn\n" +
				"  group by s.age_value, s.total, s.rn\n" +
				")\n" +
				"select 103 as analysis_id,\n" +
				"  o.total as count_value,\n" +
				"        o.min_value,\n" +
				"        o.max_value,\n" +
				"        o.avg_value,\n" +
				"        o.stdev_value,\n" +
				"        MIN(case when p.accumulated >= .50 * o.total then age_value end) as median_value,\n" +
				"        MIN(case when p.accumulated >= .10 * o.total then age_value end) as p10_value,\n" +
				"        MIN(case when p.accumulated >= .25 * o.total then age_value end) as p25_value,\n" +
				"        MIN(case when p.accumulated >= .75 * o.total then age_value end) as p75_value,\n" +
				"        MIN(case when p.accumulated >= .90 * o.total then age_value end) as p90_value\n" +
				"INTO #tempResults\n" +
				"from ageStatsPrior p\n" +
				"CROSS JOIN overallStats o\n" +
				"GROUP BY o.total, o.min_value, o.max_value, o.avg_value, o.stdev_value\n" +
				";";
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
