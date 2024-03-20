/*******************************************************************************
 * Copyright 2024 Observational Health Data Sciences and Informatics
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package org.ohdsi.sql;

import org.ohdsi.sql.SqlTranslate.Block;
import org.ohdsi.sql.SqlTranslate.MatchedPattern;

import java.sql.*;
import java.util.*;

public class BigQuerySparkTranslate {

	/**
	 * Iterates the elements of a comma-separated list of expressions (SELECT, GROUP BY, or ORDER BY).
	 */
	private static class CommaListIterator {
		private String						expressionList;
		private List<Block>					listElementPattern;
		private SqlTranslate.MatchedPattern	currentMatch;
		private ListType					listType;
		private String						expressionPrefix;
		private String						expressionSuffix;
		private String						listPrefix;
		private String						listSuffix;

		public enum ListType {
			SELECT, GROUP_BY, ORDER_BY, WITH_COLUMNS, IN
		}

		public CommaListIterator(String expression_list, ListType list_type) {
			listType = list_type;
			expressionList = expression_list;
			splitList();

			expressionList = "," + expressionList + ",";
			listElementPattern = SqlTranslate.parseSearchPattern(", @@a ,");

			currentMatch = SqlTranslate.search(expressionList, listElementPattern, 0);
			if (currentMatch.start != -1) {
				splitExpression();
			}
		}

		public boolean IsDone() {
			return currentMatch.start == -1;
		}

		public void Next() {
			// Finds the start token so that the next match doesn't overlap the current match
			final int expr_length = StringUtils.tokenizeSql(expressionList.substring(currentMatch.start, currentMatch.end)).size();
			final int startToken = currentMatch.startToken + expr_length - 1;

			// Searches using the start token from above.
			currentMatch = SqlTranslate.search(expressionList, listElementPattern, startToken);
			if (currentMatch.start != -1) {
				splitExpression();
			}
		}

		public String GetExpressionPrefix() {
			return expressionPrefix;
		}

		public String GetExpressionSuffix() {
			return expressionSuffix;
		}

		public String GetFullExpression() {
			return expressionPrefix + expressionSuffix;
		}

		public String GetListPrefix() {
			return listPrefix;
		}

		public String GetListSuffix() {
			return listSuffix;
		}

		/**
		 * Splits elements into listPrefix and listSuffix that aren't part of the list itself but may get matched as part of it.
		 */
		private void splitList() {
			listPrefix = "";
			listSuffix = "";
			switch (listType) {
				case SELECT:
					splitSelect();
					break;
				case GROUP_BY:
					splitGroupBy();
					break;
				case ORDER_BY:
					// empty
					break;
				case WITH_COLUMNS:
					// empty
					break;
				case IN:
					// empty
					break;
			}
		}

		/**
		 * The SELECT may be preceded by DISTINCT or suffixed by INTO #temptable
		 */
		private void splitSelect() {
			// Leading DISTINCT
			MatchedPattern match = SqlTranslate.search("^" + expressionList + "$", SqlTranslate.parseSearchPattern("^ distinct @@a $"), 0);
			if (match.start != -1) {
				listPrefix = "distinct ";
				expressionList = match.variableToValue.get("@@a");
			}

			// Trailing INTO #temptable
			match = SqlTranslate.search("^" + expressionList + "$", SqlTranslate.parseSearchPattern("^@@a into @@b$"), 0);
			if (match.start != -1) {
				expressionList = match.variableToValue.get("@@a");
				listSuffix = " into " + match.variableToValue.get("@@b");
			}
		}

		/**
		 * The last element of a GROUP BY element may have the ORDER BY clause appended.
		 */
		private void splitGroupBy() {
			MatchedPattern match = SqlTranslate.search("^" + expressionList + "$", SqlTranslate.parseSearchPattern("^@@a order by @@b$"), 0);
			if (match.start != -1) {
				expressionList = match.variableToValue.get("@@a");
				listSuffix = " order by " + match.variableToValue.get("@@b");
			}
		}

		private void splitExpression() {
			expressionPrefix = currentMatch.variableToValue.get("@@a");
			expressionSuffix = "";
			switch (listType) {
				case SELECT:
					splitAlias();
					break;
				case GROUP_BY:
					// empty
					break;
				case ORDER_BY:
					splitOrderElement();
					break;
				case WITH_COLUMNS:
					// empty
					break;
				case IN:
					// empty
					break;
			}
		}

		/**
		 * For a SELECT list expression, separates the main expression from the alias if it exists.
		 */
		private void splitAlias() {
			List<StringUtils.Token> tokens = StringUtils.tokenizeSql(expressionPrefix);

			// Tries to match alias
			final List<Block> alias_pattern = SqlTranslate.parseSearchPattern("^ @@a as @@b $");
			final MatchedPattern alias_match = SqlTranslate.search("^" + expressionPrefix + "$", alias_pattern, 0);
			if (alias_match.start == -1) {
				// No AS in the expression. Use heuristics to determine if the final identifier is an alias.
				if (tokens.size() >= 2) {
					StringUtils.Token possible_alias = tokens.get(tokens.size() - 1);
					String preceding_token = tokens.get(tokens.size() - 2).text;
					if (possible_alias.isIdentifier() && !preceding_token.equalsIgnoreCase(".") && !preceding_token.equalsIgnoreCase("+")) {
						expressionPrefix = expressionPrefix.substring(0, possible_alias.start);
						expressionSuffix = possible_alias.text;
					}
				}
			} else {
				expressionPrefix = alias_match.variableToValue.get("@@a");
				expressionSuffix = alias_match.variableToValue.get("@@b");
			}
		}

		/**
		 * ORDER BY elements can optionally end with ASC or DESC
		 */
		private void splitOrderElement() {
			List<StringUtils.Token> tokens = StringUtils.tokenizeSql(GetFullExpression());
			final StringUtils.Token last_token = tokens.get(tokens.size() - 1);
			if (last_token.text.equalsIgnoreCase("asc") || last_token.text.equalsIgnoreCase("desc")) {
				expressionPrefix = GetFullExpression().substring(0, last_token.start - 1);
				expressionSuffix = " " + last_token.text;
			}
		}

		/**
		 * Checks if the current expression is a column reference and nothing else
		 *
		 * @return true if the expression is a single column reference
		 */
		public boolean IsSingleColumnReference() {
			List<StringUtils.Token> tokens = StringUtils.tokenizeSql(GetExpressionPrefix());
			return tokens.size() == 3 && tokens.get(0).isIdentifier() && tokens.get(1).text.contentEquals(".") && tokens.get(2).isIdentifier();
		}
	}

	/**
	 * Removes named column lists from common table expressions and replaces them with aliases on the select list elements.
	 *
	 * @param sql
	 *            - the query to transform
	 * @return the query after transformation
	 */
	private static String bigQueryAliasCommonTableExpressions(String sql, String pattern) {
		List<Block> cte_pattern = SqlTranslate.parseSearchPattern(pattern);

		// Iterates over common table expressions with column lists
		for (MatchedPattern cte_match = SqlTranslate.search(sql, cte_pattern, 0); cte_match.start != -1; cte_match = SqlTranslate.search(sql, cte_pattern,
				cte_match.startToken + 1)) {

			CommaListIterator with_list_iter = new CommaListIterator(cte_match.variableToValue.get("@@b"), CommaListIterator.ListType.WITH_COLUMNS);
			CommaListIterator select_list_iter = new CommaListIterator(cte_match.variableToValue.get("@@c"), CommaListIterator.ListType.SELECT);
			String replacement_select_list = "";

			// Iterates the common table expression column list and the SELECT list in parallel
			while (!with_list_iter.IsDone()) {
				if (select_list_iter.IsDone()) {
					break;
				}
				final String with_expr = with_list_iter.GetFullExpression();
				final String select_expr = select_list_iter.GetExpressionPrefix() + " as " + with_expr;
				if(replacement_select_list.length() > 0) {
                    replacement_select_list = replacement_select_list + ",";
                }
				replacement_select_list = replacement_select_list + select_expr;
				with_list_iter.Next();
				select_list_iter.Next();
			}
			replacement_select_list = select_list_iter.GetListPrefix() + replacement_select_list + select_list_iter.GetListSuffix();

			sql = sql.substring(0, cte_match.start)
				+ pattern
					.replace("@@a", cte_match.variableToValue.get("@@a"))
					.replace("(@@b)", "")
					.replace("@@c", replacement_select_list)
					.replace("@@d", nullToEmptyString(cte_match.variableToValue.get("@@d")))
				+ sql.substring(cte_match.end);
		}
		return sql;
	}

	private static String nullToEmptyString(String string) {
	  if (string == null)
		  return "";
	  else
		  return string;
	}
	/**
	 * Finds complex expressions in a GROUP BY or ORDER BY list and replaces them with references to matching select list expressions.
	 *
	 * @param sql
	 *            - the query to transform
	 * @param select_pattern
	 *            - pattern to find the SELECTs, @@s is the select list, @@r is the list to convert
	 * @param list_type
	 *            - CommaListSeparator.ListType for the list to replace
	 * @return the query with GROUP BY elements replaced
	 */
	private static String bigQueryConvertSelectListReferences(String sql, String select_pattern, CommaListIterator.ListType list_type) {
		// Iterates SELECT statements
		List<Block> select_statement_pattern = SqlTranslate.parseSearchPattern(select_pattern);
		for (MatchedPattern select_statement_match = SqlTranslate.search(sql, select_statement_pattern,
				0); select_statement_match.start != -1; select_statement_match = SqlTranslate.search(sql, select_statement_pattern,
						select_statement_match.startToken + 1)) {
			final String select_list = select_statement_match.variableToValue.get("@@s");
			final String list_to_replace = select_statement_match.variableToValue.get("@@r");
			String replacement_list = "";

			// Iterates the list to replace
			CommaListIterator list_to_replace_iter = new CommaListIterator(list_to_replace, list_type);
			for (; !list_to_replace_iter.IsDone(); list_to_replace_iter.Next()) {
				final String list_expr = list_to_replace_iter.GetExpressionPrefix();
				final String list_expr_suffix = list_to_replace_iter.GetExpressionSuffix();
				List<Block> list_expr_pattern = SqlTranslate.parseSearchPattern(list_expr);
				if (list_to_replace_iter.IsSingleColumnReference()) {
					// Copy single column references directly
					replacement_list = replacement_list + ", " + list_expr + list_expr_suffix;
				} else {
					// Iterates the SELECT list searching for a matching expression
					CommaListIterator select_list_iter = new CommaListIterator(select_list, CommaListIterator.ListType.SELECT);
					boolean found = false;
					for (int i = 1; !select_list_iter.IsDone(); ++i, select_list_iter.Next()) {
						final String select_expr = select_list_iter.GetExpressionPrefix();
						if (SqlTranslate.search(select_expr, list_expr_pattern, 0).start != -1) {
							found = true;
							replacement_list = replacement_list + ", " + i + list_expr_suffix;
							break;
						}
					}
					if (!found) {
						// No matches. Fall back to copying the expression directly
						replacement_list = replacement_list + ", " + list_expr + list_expr_suffix;
					}
				}
			}
			replacement_list = list_to_replace_iter.GetListPrefix() + replacement_list.substring(1) + list_to_replace_iter.GetListSuffix();

			// Copies everything from the match except for the replacement list
			final String suffix = sql.substring(select_statement_match.end);
			sql = sql.substring(0, select_statement_match.start);
			for (int i = 0; i < select_statement_pattern.size(); ++i) {
				if (sql.length() > 0) {
					sql += " ";
				}
				Block block = select_statement_pattern.get(i);
				if (block.isVariable) {
					if (block.text.equals("@@r")) {
						sql += replacement_list;
					} else {
						sql += select_statement_match.variableToValue.get(block.text);
					}
				} else {
					sql += block.text;
				}
			}
			sql += suffix;
		}

		return sql;
	}

	/**
	 * Lower cases everything but string literals
	 *
	 * @param sql - the query to translate
	 * @return the query after translation
	 */
	private static String bigQueryLowerCase(String sql) {
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(sql);
		for (StringUtils.Token token : tokens) {
			if (!token.inQuotes && !token.text.startsWith("@")) {
				sql = sql.substring(0, token.start) + token.text.toLowerCase() + sql.substring(token.end);
			}
		}
		return sql;
	}

	/**
	 * bigQuery specific translations
	 *
	 * @param sql
	 *            - the query to translate
	 * @return the query after translation
	 */
	public static String translatebigQuery(String sql) {
		sql = bigQueryLowerCase(sql);
		sql = bigQueryAliasCommonTableExpressions(sql, "with @@a (@@b) as (select @@c from @@d)");
		sql = bigQueryAliasCommonTableExpressions(sql, "with @@a (@@b) as (select @@c union @@d)");
		sql = bigQueryAliasCommonTableExpressions(sql, "with @@a (@@b) as (select @@c)");
		sql = bigQueryAliasCommonTableExpressions(sql, ", @@a (@@b) as (select @@c from @@d)");

		String groupByReferences = "select @@s from @@b group by @@r";
		sql = bigQueryConvertSelectListReferences(sql, groupByReferences + ";", CommaListIterator.ListType.GROUP_BY);
		sql = bigQueryConvertSelectListReferences(sql, groupByReferences + ")", CommaListIterator.ListType.GROUP_BY);
		sql = bigQueryConvertSelectListReferences(sql, groupByReferences + " having", CommaListIterator.ListType.GROUP_BY);
		sql = bigQueryConvertSelectListReferences(sql, groupByReferences + " order by", CommaListIterator.ListType.GROUP_BY);

		String orderBy = "select @@s from @@b order by @@r";
		sql = bigQueryConvertSelectListReferences(sql, orderBy + ";", CommaListIterator.ListType.ORDER_BY);
		sql = bigQueryConvertSelectListReferences(sql, orderBy + ")", CommaListIterator.ListType.ORDER_BY);

		return sql;
	}

	// Spark functions -------

	private static List<String> getMetaFields(String target_table_name, Connection connection) throws SQLException {

		Statement statement = connection.createStatement();
		ResultSet rs = statement.executeQuery("show columns in " + target_table_name);

		List<String> metaFields = new ArrayList<String>();
		while (rs.next()) {
			metaFields.add(rs.getString("COL_NAME").toLowerCase());
		}

		return metaFields;
	}

	private static String sparkCreateTable(String sql) {
		if (!sql.endsWith(";")) {
			sql += ";";
		}

		String pattern = "CREATE TABLE @@table (@@definition)";

		List<Block> create_table_pattern = SqlTranslate.parseSearchPattern(pattern);

		sql = sql.trim().replaceAll("\t", " ").replaceAll(" +", " ");

		MatchedPattern create_table_match = SqlTranslate.search(sql, create_table_pattern, 0);

		String table_name = create_table_match.variableToValue.get("@@table");
		String definition_list = create_table_match.variableToValue.get("@@definition");

		if (table_name != null && definition_list != null) {

			table_name = table_name.replaceAll("\r\n", "");
			definition_list = definition_list.toLowerCase().replaceAll("\r\n", "").replaceAll(" as ", " ");

			List<String> column_names = new ArrayList<String>();
			for (String f : definition_list.split(",")) {
				column_names.add("\tCAST(NULL AS " + f.trim().split(" ")[1] + ") AS " + f.trim().split(" ")[0]);
			}

			String prefix = sql.substring(0, create_table_match.start);

			sql = prefix
					+ "SELECT "
					+ String.join(",\r\n", column_names) + " INTO "
					+ table_name + " WHERE 1 = 0";
		}

		return sql.replaceAll(";", "");
	}

		private static String sparkInsertGetValueMappings(MatchedPattern insert_match, List<String> metaFields) {
			CommaListIterator insert_list_iter = new CommaListIterator(insert_match.variableToValue.get("@@columns"), CommaListIterator.ListType.WITH_COLUMNS);
			CommaListIterator select_list_iter = new CommaListIterator(insert_match.variableToValue.get("@@remainder"), CommaListIterator.ListType.SELECT);

			Map<String, String> mappings = new HashMap<String, String>();

			// Iterates the insert column list and the SELECT list in parallel
			while (!insert_list_iter.IsDone()) {
				if (select_list_iter.IsDone()) {
					break;
				}
				final String column_expr = insert_list_iter.GetFullExpression().toLowerCase();
				String select_expr = select_list_iter.GetExpressionPrefix();

				mappings.put(column_expr.trim(), select_expr.trim());

				insert_list_iter.Next();
				select_list_iter.Next();
			}

			// re-construct the definitions to explicitly match table structure
			List<String> definition_sql = new ArrayList<String>();
			for (String mf : metaFields) {
				if (mappings.containsKey(mf)) {
					definition_sql.add(mappings.get(mf));
				} else {
					definition_sql.add("NULL");
				}
			}
			String all_select_sql = String.join(",\r\n", definition_sql);
			return all_select_sql;
		}

		private static String sparkInsertGetSelectMappings(List<String> metaFields, MatchedPattern insert_match) {

			CommaListIterator insert_list_iter = new CommaListIterator(insert_match.variableToValue.get("@@columns"), CommaListIterator.ListType.WITH_COLUMNS);

			List<String> column_names = new ArrayList<>();

			// Iterates the given insert column list
			while (!insert_list_iter.IsDone()) {
				final String column_expr = insert_list_iter.GetFullExpression().toLowerCase();
				column_names.add(column_expr.trim());
				insert_list_iter.Next();
			}

			// re-construct the definitions to explicitly match table structure
			List<String> definition_sql = new ArrayList<String>();
			for (String mf : metaFields) {
				if (column_names.contains(mf)) {
					definition_sql.add(column_names.get(column_names.indexOf(mf)));
				} else {
					definition_sql.add("NULL AS " + mf);
				}
			}

			return String.join(",\r\n", definition_sql);
		}

	/**
	 * Handles insert into commands by checking table metadata
	 * then writes a create delta table syntax for Spark.
	 * This is necessary due to limitation in Spark that prevents inserts without the full table specification from working.
	 * Needed by Atlas for cohort logic.
	 *
	 * @param sql - the query to translate
	 * @param connectionString - a JDBC connection string
	 * @return the query after translation
	 */
	public static String sparkHandleInsert(String sql, String connectionString) throws SQLException {
		Connection connection = DriverManager.getConnection(connectionString);
		return sparkHandleInsert(sql, connection);
	}

	/**
	 * Handles insert into commands by checking table metadata
	 * then writes a create delta table syntax for Spark.
	 * This is necessary due to limitation in Spark that prevents inserts without the full table specification from working.
	 * Needed by Atlas for cohort logic.
	 *
	 * @param sql - the query to translate
	 * @param connection - a Java connection object
	 * @return the query after translation
	 */
	public static String sparkHandleInsert(String sql, Connection connection) throws SQLException {

		List<String> splits = new ArrayList<String>(Arrays.asList(SqlSplit.splitSql(sql)));

		for (int i = 0; i < splits.size(); i++) {
			splits.set(i, sparkInsert(splits.get(i), connection));
		}

		splits.removeAll(Arrays.asList("", null));
		if (splits.size() > 1 || sql.trim().endsWith(";")) {
			sql = String.join(";\r\n", splits).trim() + ";";
		} else {
			sql = String.join(";\r\n", splits).trim();
		}

		return sql;
	}

	private static String sparkInsert(String sql, Connection connection) throws SQLException {
		sql = sql.trim().replaceAll("\t", " ").replaceAll(" +", " ");

		if (!sql.trim().endsWith(";")) {
			sql += ";";
		}

		String this_pattern = "INSERT INTO @@target (@@columns) VALUES (@@remainder);";
		List<Block> insert_pattern = SqlTranslate.parseSearchPattern(this_pattern);
		MatchedPattern insert_match = SqlTranslate.search(sql, insert_pattern, 0);

		String target_table = insert_match.variableToValue.get("@@target");
		String columns = insert_match.variableToValue.get("@@columns");
		String remainder = insert_match.variableToValue.get("@@remainder");

		if (target_table != null
				&& columns != null
				&& remainder != null) {

			target_table = target_table.replaceAll("\r\n", "");

			List<String> metaFields;
			try {
				metaFields = getMetaFields(target_table, connection);
			} catch (SQLException e) {
				return sql;
			}

			String mappings = sparkInsertGetValueMappings(insert_match, metaFields);

			sql = "INSERT INTO " + target_table + "\r\n" + "VALUES\r\n" +
					"(" + mappings + ")";

			return sql.replaceAll(";", "");
		}

		this_pattern = "INSERT INTO @@target (@@columns) @@remainder;";
		insert_pattern = SqlTranslate.parseSearchPattern(this_pattern);
		insert_match = SqlTranslate.search(sql, insert_pattern, 0);

		target_table = insert_match.variableToValue.get("@@target");
		remainder = insert_match.variableToValue.get("@@remainder");
		columns = insert_match.variableToValue.get("@@columns");

		if ((target_table != null) &&
				(remainder != null) &&
				(columns != null)) {

			// attempt to get table metadata
			List<String> metaFields;
			try {
				metaFields = getMetaFields(target_table, connection);
			} catch (SQLException e) {
				return sql;
			}

			String mappings = sparkInsertGetSelectMappings(metaFields, insert_match);

			sql = "with cte_wrapper (" + columns + ")\r\n as \r\n (\r\n" + remainder + "\r\n) \r\n" +
					"INSERT INTO " + target_table + "\r\n select " + mappings + "\r\nfrom cte_wrapper";
		}

		return sql.replaceAll(";", "");

	}

	/**
	 * spark specific operations
	 *
	 * @param sql - the query to translate
	 * @return the query after translation
	 * @throws SQLException
	 */
	public static String translateSpark(String sql) {
		String[] splits = SqlSplit.splitSql(sql);

		// translate create table statements
		for (int i = 0; i < splits.length; i++) {
			splits[i] = sparkCreateTable(splits[i]);
		}

		// if this is a batch command or the SQL originally ended with a semicolon
		// ensure the semicolon is back in the SQL

		if (splits.length > 1 || sql.trim().endsWith(";")) {
			sql = String.join(";\r\n", splits).trim() + ";";
		} else {
			sql = String.join(";\r\n", splits).trim();
		}
		return sql;
	}
}
