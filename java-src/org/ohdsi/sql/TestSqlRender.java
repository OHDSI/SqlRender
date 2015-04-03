package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {
		String sql = "{false | false}?{true}:{false}";
		sql = SqlRender.renderSql(sql, null, null);
		System.out.println(sql);
		
//		String[] parameters = new String[]{"a","ab"};
//		String[] values = new String[]{"x","y"};
//		String sql = "SELECT * FROM table WHERE x = @ab AND {@a == 'blaat'}?{y = 1234}:{x = 1};";
////		System.out.println(sql);
//		sql = SqlRender.renderSql(sql, parameters,values);
//		System.out.println(sql);
//		
////		String sql = "SELECT * FROM x; DROP TABLE y";
//		for (String str : SqlSplit.splitSql(sql))
//			System.out.println(str + "---");
		
//		String sql = "DROP TABLE #a;\nDROP TABLE #b;";
//		String source = "sql server";
//		String target = "pdw";
//		System.out.println(SqlTranslate.translateSql(sql, source, target,"C:/Users/mschuemi/git/SqlRender/inst/csv/replacementPatterns.csv"));
	}
}
