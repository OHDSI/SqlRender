package org.ohdsi.sql;

public class TestSqlRender {

	public static void main(String[] args) {
		//String sql = "{false | false | 1 IN (1,2,3)}?{true}:{false}";
		String sql = "-- { 1500 in (@list_of_analysis_ids) | 1501 in (@list_of_analysis_ids) | 1502 in (@list_of_analysis_ids) | 1503 in (@list_of_analysis_ids) | 1504 in (@list_of_analysis_ids) | 1505 in (@list_of_analysis_ids) | 1506 in (@list_of_analysis_ids) | 1507 in (@list_of_analysis_ids) | 1508 in (@list_of_analysis_ids) | 1509 in (@list_of_analysis_ids) | 1510 in (@list_of_analysis_ids) | 1511 in (@list_of_analysis_ids)}?{HERE!}";
		sql = SqlRender.renderSql(sql, new String[]{"list_of_analysis_ids"}, new String[]{"1600,1601,1602,1603,1604,1605,1606,1607,1608,1609,1610"});
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
