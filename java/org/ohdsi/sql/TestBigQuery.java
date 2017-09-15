package org.ohdsi.sql;
import java.io.IOException;
import java.io.FileReader;
import java.io.BufferedReader;

public class TestBigQuery {

	static private String readFile(String file) throws IOException {
		BufferedReader reader = new BufferedReader(new FileReader (file));
		String         line = null;
		StringBuilder  stringBuilder = new StringBuilder();
		String         ls = System.getProperty("line.separator");

		try {
			while((line = reader.readLine()) != null) {
				stringBuilder.append(line);
				stringBuilder.append(ls);
			}

			return stringBuilder.toString();
		} finally {
			reader.close();
		}
	}

	public static void main(String[] args) throws IOException {
		
		//String sql = "--HINT DISTRIBUTE_ON_KEY(analysis_id)\nCREATE TABLE results.achilles_results_dist";
		//for (String part : SqlSplit.splitSql(sql))
			//System.out.println(part);
		
//		Pattern pattern = Pattern.compile("^((?!FROM).)*$");
//		System.out.println(pattern.matcher("SELECT * blaat b;").matches());
		
//		String sql = "SELECT * FROM table {@a = true} ?  {WHERE name = '@name'};";
//		sql = SqlRender.renderSql(sql, new String[]{"name", "a"}, new String[]{"NA\\joe", "true"});
//		System.out.println(sql);

		String sourceSql;
		String path = "inst/csv/replacementPatterns.csv";
		/*
		try {
			sourceSql = readFile("../achilles-scripts/output-heel-rendered.sql");
		} catch (IOException e) {
			throw e;
		}
		*/
		sourceSql = "select coalesce(x, 0), coalesce(12, y) from t;";

		String translated_sql;
		translated_sql = SqlTranslate.translateSqlWithPath(sourceSql, "bigquery", null, null, path);
		System.out.println(translated_sql);

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
