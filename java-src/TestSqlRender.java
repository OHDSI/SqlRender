
public class TestSqlRender {

	public static void main(String[] args) {
//		String[] parameters = new String[]{"a"};
//		String[] values = new String[]{"x"};
//		String sql = "{DEFAULT @a = '123'} SELECT * FROM table WHERE x = @a AND {@b == 'blaat'}?{y = 1234}:{x = 1};";
//		System.out.println(sql);
//		sql = SqlRender.renderSql(sql, parameters,values);
//		System.out.println(sql);
		
//		String sql = "SELECT * FROM x; DROP TABLE y";
//		for (String str : SqlSplit.splitSql(sql))
//			System.out.println(str + "---");
		
		String sql = "DROP TABLE #a;\nDROP TABLE #b;";
		String source = "sql server";
		String target = "pdw";
		System.out.println(SqlTranslate.translateSql(sql, source, target,"C:/Users/mschuemi/git/SqlRender/inst/csv/replacementPatterns.csv"));
	}
}
