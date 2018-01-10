/*******************************************************************************
 * Copyright 2018 Observational Health Data Sciences and Informatics
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

import java.util.List;

import org.ohdsi.sql.SqlTranslate.Block;
import org.ohdsi.sql.SqlTranslate.MatchedPattern;

public class BigQueryTranslate {

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
	private static String bigQueryAliasCommonTableExpressions(String sql, String pattern, String prefix) {
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
				replacement_select_list = replacement_select_list + "," + select_expr;
				with_list_iter.Next();
				select_list_iter.Next();
			}
			replacement_select_list = select_list_iter.GetListPrefix() + replacement_select_list + select_list_iter.GetListSuffix();

			sql = sql.substring(0, cte_match.start) + prefix + cte_match.variableToValue.get("@@a") + " as (select "
					+ replacement_select_list.substring(1) + " from " + cte_match.variableToValue.get("@@d") + ")" + sql.substring(cte_match.end);
		}
		return sql;
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
		// Adds semi-colon in case there isn't one already
		sql = sql + ";";

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

		// Removes semi-colon added at the beginning
		sql = sql.substring(0, sql.length() - 1);
		return sql;
	}

	/**
	 * Decides if a select list expression is a string concatenation.
	 *
	 * @param select_expr
	 *            - the expression to evaluate
	 * @return true if the expression is a string concatenation
	 */
	private static boolean bigQueryExprIsStringConcat(String select_expr) {
		// Additional patterns can be added to this list to heuristically recognize string concatenation expressions
		final String[] concat_exprs = { "^ '@@a' +", "^ cast(@@a as varchar) +", "^ isnull(@@a, '@@b') +", "^ case @@a then '@@b' @@c end +", "^ @@a + '@@b'" };
		for (int i = 0; i < concat_exprs.length; ++i) {
			MatchedPattern concat_match = SqlTranslate.search("^" + select_expr, SqlTranslate.parseSearchPattern(concat_exprs[i]), 0);
			if (concat_match.start != -1) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Converts a string expression to use CONCAT() rather than +
	 *
	 * @param select_expr
	 *            - the expression to transform
	 * @return the transformed expression
	 */
	private static String bigQueryReplaceStringConcatsInExpr(String select_expr) {
		select_expr = "+" + select_expr + "+";

		// Repeatedly replace + with CONCAT until there are none left
		List<Block> concat_pattern = SqlTranslate.parseSearchPattern("+ @@a + @@b +");
		while (true) {
			MatchedPattern concat_match = SqlTranslate.search(select_expr, concat_pattern, 0);
			if (concat_match.start == -1) {
				break;
			}
			String suffix = "";
			if (concat_match.end < select_expr.length()) {
				suffix = select_expr.substring(concat_match.end + 1);
			}
			select_expr = "+concat(" + concat_match.variableToValue.get("@@a") + ", " + concat_match.variableToValue.get("@@b") + ")+" + suffix;
		}
		return select_expr.substring(1, select_expr.length() - 1);
	}

	/**
	 * Converts string concatenations using the + operator to CONCAT() calls.
	 *
	 * @param sql
	 *            - the query to transform
	 * @return the query with string concatenations converted
	 */
	private static String bigQueryReplaceStringConcatsInStatement(String sql) {
		List<Block> select_statement_pattern = SqlTranslate.parseSearchPattern("select @@a from");

		// Iterates SELECT statements
		for (MatchedPattern select_statement_match = SqlTranslate.search(sql, select_statement_pattern,
				0); select_statement_match.start != -1; select_statement_match = SqlTranslate.search(sql, select_statement_pattern,
						select_statement_match.startToken + 1)) {
			final String select_list = select_statement_match.variableToValue.get("@@a");
			String replacement_select_list = "";

			// Iterates elements of the select list
			CommaListIterator select_list_iter = new CommaListIterator(select_list, CommaListIterator.ListType.SELECT);
			for (; !select_list_iter.IsDone(); select_list_iter.Next()) {

				// Replaces all string concatenations
				String replacement_select_expr = select_list_iter.GetExpressionPrefix();
				if (bigQueryExprIsStringConcat(replacement_select_expr)) {
					replacement_select_expr = bigQueryReplaceStringConcatsInExpr(replacement_select_expr);
				}
				replacement_select_list += ", " + replacement_select_expr;
				final String alias = select_list_iter.GetExpressionSuffix();
				if (alias.length() > 0) {
					replacement_select_list += "as " + alias;
				}
			}
			replacement_select_list = select_list_iter.GetListPrefix() + replacement_select_list.substring(1) + select_list_iter.GetListSuffix();

			sql = sql.substring(0, select_statement_match.start) + "select " + replacement_select_list + " from "
					+ sql.substring(select_statement_match.end, sql.length());
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
			if (!token.inQuotes) {
				sql = sql.substring(0, token.start) + token.text.toLowerCase() + sql.substring(token.end);
			}
		}
		return sql;
	}

	/**
	 * Removes quotes from IN lists elements where the lhs ends with "_id" and the elements are all digits
	 *
	 * @param sql - the query to translate
	 * @return the query after translation
	 */
	private static String bigQueryUnquoteIdInLists(String sql) {
		List<Block> in_list_pattern = SqlTranslate.parseSearchPattern("in (@@i)");
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(sql);

		// Iterates SELECT statements
		for (MatchedPattern in_list_match = SqlTranslate.search(sql, in_list_pattern,
				0); in_list_match.start != -1; in_list_match = SqlTranslate.search(sql, in_list_pattern,
				in_list_match.startToken + 1)) {
			final String in_list = in_list_match.variableToValue.get("@@i");

			// Checks if the lhs is an identifier ending with "_id"
			if (in_list_match.startToken <=0) {
				continue;
			}
			final StringUtils.Token lhs_token = tokens.get(in_list_match.startToken - 1);
			if (!lhs_token.isIdentifier()) {
				continue;
			}
			if (!lhs_token.text.toLowerCase().endsWith("_id")) {
				continue;
			}

			// Iterates elements of the IN list
			CommaListIterator in_list_iter = new CommaListIterator(in_list, CommaListIterator.ListType.IN);
			for (; !in_list_iter.IsDone(); in_list_iter.Next()) {
				final String expr = in_list_iter.GetFullExpression();
				if (expr.charAt(0) != '\'' || expr.charAt(expr.length() - 1) != '\'') {
					return sql;
				}
				for (int i = 1; i < expr.length() - 1; ++i) {
					if (!Character.isDigit(expr.charAt(i))) {
						return sql;
					}
				}
			}
			final String replacement_in_list = in_list.replaceAll("\'", "");
			sql = sql.substring(0, in_list_match.start)
					+ " in (" + replacement_in_list + ")"
					+ sql.substring(in_list_match.end, sql.length());
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
		sql = bigQueryAliasCommonTableExpressions(sql, "with @@a (@@b) as (select @@c from @@d)", "with ");
		sql = bigQueryAliasCommonTableExpressions(sql, ", @@a (@@b) as (select @@c from @@d)", ", ");
		sql = bigQueryConvertSelectListReferences(sql, "select @@s from @@b group by @@r;", CommaListIterator.ListType.GROUP_BY);
		sql = bigQueryConvertSelectListReferences(sql, "select @@s from @@b group by @@r)", CommaListIterator.ListType.GROUP_BY);
		sql = bigQueryConvertSelectListReferences(sql, "select @@s from @@b group by @@c order by @@r;", CommaListIterator.ListType.ORDER_BY);
		sql = bigQueryReplaceStringConcatsInStatement(sql);
		sql = bigQueryUnquoteIdInLists(sql);
		return sql;
	}
}
