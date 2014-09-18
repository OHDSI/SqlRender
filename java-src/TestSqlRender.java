
public class TestSqlRender {

	public static void main(String[] args) {
//		String[] parameters = new String[]{"a"};
//		String[] values = new String[]{"x"};
//		String sql = "{DEFAULT @a = '123'} SELECT * FROM table WHERE x = @a AND {@b == 'blaat'}?{y = 1234}:{x = 1};";
//		System.out.println(sql);
//		sql = SqlRender.renderSql(sql, parameters,values);
//		System.out.println(sql);
		
		String sql = "SELECT * FROM x; DROP TABLE y";
		for (String str : SqlSplit.splitSql(sql))
			System.out.println(str + "---");
		
//		String sql = "IF OBJECT_ID('cohort', 'U') IS NOT NULL DROP TABLE cohort;";
//		String source = "sql server";
//		String target = "oracle";
//		System.out.println(SqlTranslate.translateSql(sql, source, target));
	}
}
