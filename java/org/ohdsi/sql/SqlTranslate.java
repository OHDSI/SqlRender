/*******************************************************************************
 * Copyright 2017 Observational Health Data Sciences and Informatics
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

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.*;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SqlTranslate {
	public static int							SESSION_ID_LENGTH				= 8;
	public static int							MAX_ORACLE_TABLE_NAME_LENGTH	= 30;
	private static Map<String, List<String[]>>	targetToReplacementPatterns		= null;
	private static ReentrantLock				lock							= new ReentrantLock();
	private static Random						random							= new Random();
	private static String						globalSessionId					= null;
	private static String						SOURCE_DIALECT					= "sql server";

	private static class Block extends StringUtils.Token {
		public boolean	isVariable;
		public String	regEx;

		public Block(StringUtils.Token other) {
			super(other);
			isVariable = false;
		}
	}

	private static class MatchedPattern {
		public int					start;
		public int					end;
		public int					startToken;
		public Map<String, String>	variableToValue	= new HashMap<String, String>();
	}

	private static List<Block> parseSearchPattern(String pattern) {
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(pattern.toLowerCase());
		List<Block> blocks = new ArrayList<Block>();
		for (int i = 0; i < tokens.size(); i++) {
			Block block = new Block(tokens.get(i));
			if (block.text.length() > 2 && block.text.charAt(0) == '@') {
				block.isVariable = true;
			}
			if (block.text.equals("@@") && i < tokens.size() - 2 && tokens.get(i + 1).text.equals("(")) {
				// Its a regex variable. Rejoin subsequent tokens
				boolean escape = false;
				int nesting = 0;
				for (int j = i + 2; j < tokens.size(); j++) {
					if (tokens.get(j).text.equals("\\"))
						escape = !escape;
					else if (!escape && tokens.get(j).text.equals("("))
						nesting++;
					else if (!escape && tokens.get(j).text.equals(")")) {
						if (nesting == 0) {
							block.text = "@@" + tokens.get(j + 1).text;
							block.regEx = pattern.substring(tokens.get(i + 1).end, tokens.get(j).start);
							block.end = tokens.get(j + 1).end;
							block.isVariable = true;
							i = j + 1;
							break;
						}
						nesting--;
					}
				}
			}
			blocks.add(block);
		}
		if ((blocks.get(0).isVariable && blocks.get(0).regEx == null)
				|| (blocks.get(blocks.size() - 1).isVariable && blocks.get(blocks.size() - 1).regEx == null)) {
			throw new RuntimeException("Error in search pattern: pattern cannot start or end with a non-regex variable: " + pattern);
		}
		return blocks;
	}

	private static MatchedPattern search(String sql, List<Block> parsedPattern, int startToken) {
		String lowercaseSql = sql.toLowerCase();
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(lowercaseSql);
		int matchCount = 0;
		int varStart = 0;
		Stack<String> nestStack = new Stack<String>();
		boolean inPatternQuote = false;
		MatchedPattern matchedPattern = new MatchedPattern();
		for (int cursor = startToken; cursor < tokens.size(); cursor++) {
			StringUtils.Token token = tokens.get(cursor);
			if (parsedPattern.get(matchCount).isVariable) {
				if (parsedPattern.get(matchCount).regEx != null && (matchCount == parsedPattern.size() - 1 || parsedPattern.get(matchCount + 1).isVariable)) {
					// Regex variable at end of pattern, or has another variable following it
					Pattern pattern = Pattern.compile(parsedPattern.get(matchCount).regEx, Pattern.CASE_INSENSITIVE);
					Matcher matcher = pattern.matcher(sql.substring(token.start));
					if (matcher.find() && matcher.start() == 0) {
						if (matchCount == 0) {
							matchedPattern.start = token.start;
							matchedPattern.startToken = cursor;
						}
						matchedPattern.variableToValue.put(parsedPattern.get(matchCount).text, sql.substring(token.start, token.start + matcher.end()));
						matchCount++;
						if (matchCount == parsedPattern.size()) {
							matchedPattern.end = token.start + matcher.end();
							return matchedPattern;
						} else if (parsedPattern.get(matchCount).isVariable) {
							varStart = token.start + matcher.end();
						}
						// Fast forward cursor to after matched patterns:
						while (cursor < tokens.size() && tokens.get(cursor).start < token.start + matcher.end())
							cursor++;
					} else {
						matchCount = 0;
					}
				} else if (nestStack.size() == 0 && matchCount < parsedPattern.size() - 1 && token.text.equals(parsedPattern.get(matchCount + 1).text)) {
					// Found the token after the variable
					if (parsedPattern.get(matchCount).regEx != null && !matches(parsedPattern.get(matchCount).regEx, sql.substring(varStart, token.start))) {
						// Content didn't match regex
						matchCount = 0;
						cursor = matchedPattern.startToken;
					} else {
						// No regex or matched regex
						matchedPattern.variableToValue.put(parsedPattern.get(matchCount).text, sql.substring(varStart, token.start));
						matchCount += 2;
						if (matchCount == parsedPattern.size()) {
							matchedPattern.end = token.end;
							return matchedPattern;
						} else if (parsedPattern.get(matchCount).isVariable) {
							varStart = (cursor < tokens.size() - 1)?tokens.get(cursor+1).start:-1;
						}
						if (token.text.equals("'") || token.text.equals("'"))
							inPatternQuote = !inPatternQuote;
					}
				} else if (nestStack.size() == 0 && !inPatternQuote && (token.text.equals(";") || token.text.equals(")"))) { // Not allowed to span multiple SQL
					// statements or outside of nesting
					matchCount = 0;
					cursor = matchedPattern.startToken;
				} else {
					if (nestStack.size() != 0 && (nestStack.peek().equals("\"") || nestStack.peek().equals("'"))) { // inside quoted string
						if (token.text.equals(nestStack.peek()))
							nestStack.pop();
					} else {
						if (token.text.equals("\"") || token.text.equals("'")) {
							nestStack.push(token.text);
						} else if (!inPatternQuote && token.text.equals("(")) {
							nestStack.push(token.text);
						} else if (!inPatternQuote && nestStack.size() != 0 && token.text.equals(")") && nestStack.peek().equals("(")) {
							nestStack.pop();
						}
					}
				}
			} else {
				// Check if token matches current part of pattern. But first part cannot be within quotes:
				if (token.text.equals(parsedPattern.get(matchCount).text) && (matchCount != 0 || !token.inQuotes)) {
					if (matchCount == 0) {
						matchedPattern.start = token.start;
						matchedPattern.startToken = cursor;
					}
					matchCount++;
					if (matchCount == parsedPattern.size()) {
						matchedPattern.end = token.end;
						return matchedPattern;
					} else if (parsedPattern.get(matchCount).isVariable) {
						varStart = (cursor < tokens.size() - 1)?tokens.get(cursor+1).start:-1;
					}
					if (token.text.equals("'") || token.text.equals("\""))
						inPatternQuote = !inPatternQuote;
				} else if (matchCount != 0) {
					matchCount = 0;
					cursor = matchedPattern.startToken;
				}
			}
			if (matchCount != 0 && cursor == tokens.size() - 1) { // If at end of sql and still didn't finish pattern, we're not going to finish it
				matchCount = 0;
				cursor = matchedPattern.startToken;
			}
		}
		matchedPattern.start = -1;
		return matchedPattern;
	}

	private static boolean matches(String regex, String string) {
		Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(string);
		return (matcher.matches());
	}

	private static String searchAndReplace(String sql, List<Block> parsedPattern, String replacePattern) {
		MatchedPattern matchedPattern = search(sql, parsedPattern, 0);
		while (matchedPattern.start != -1) {
			String replacement = replacePattern;
			for (Map.Entry<String, String> pair : matchedPattern.variableToValue.entrySet())
				replacement = StringUtils.replaceAll(replacement, pair.getKey(), pair.getValue());
			sql = sql.substring(0, matchedPattern.start) + replacement + sql.substring(matchedPattern.end, sql.length());
			int delta = 1;
			if (StringUtils.tokenizeSql(replacement).size() == 0)
				delta = 0;
			matchedPattern = search(sql, parsedPattern, matchedPattern.startToken + delta);
		}
		return sql;
	}

	/**
	 * Removes named column lists from common table expressions and replaces them with aliases on the
	 * select list elements.
	 *
	 * @param sql - the query to transform
	 * @return the query after transformation
	 */
	private static String bigQueryAliasCommonTableExpressions(String sql) {
		List<Block> cte_pattern = parseSearchPattern("@@(with|,)p @@a (@@b) as (select @@c from @@d)");
		MatchedPattern cte_match = search(sql, cte_pattern, 0);

		// Iterates over common table expressions with column lists
		while (cte_match.start != -1) {
			final String with_list = "," + cte_match.variableToValue.get("@@b") + ",";
			final String select_list = "," + cte_match.variableToValue.get("@@c") + ",";
			String replacement_select_list = "";

			// Iterates the common table expression column list and the SELECT list in parallel
			MatchedPattern with_match = search(with_list, parseSearchPattern(", @@a ,"), 0);
			MatchedPattern select_match = search(select_list, parseSearchPattern(", @@a ,"), 0);
			while (with_match.start != -1) {
				if (select_match.start == -1) {
					break;
				}
				final String with_expr = with_match.variableToValue.get("@@a");
				String select_expr = "," + select_match.variableToValue.get("@@a") + ",";
				final MatchedPattern select_alias = search(select_expr, parseSearchPattern(", @@a as @@b ,"), 0);
				if (select_alias.start != -1) {
					final String alias = select_alias.variableToValue.get("@@b");
					if (alias.trim().equalsIgnoreCase(with_expr.trim())) {
						// SELECT list alias already matches the column list
						select_expr = select_expr.substring(0, select_expr.length() - 1);
					} else {
						// SELECT list exists but is different than the column list.  Replace it
						select_expr = "," + select_alias.variableToValue.get("@@a") + " as " + with_expr;
					}
				} else {
					// No existing SELECT list alias.  Add one
					select_expr = select_expr.substring(0, select_expr.length() - 1) + " as " + with_expr;
				}
				replacement_select_list = replacement_select_list + select_expr;
				with_match = search(with_list, parseSearchPattern(", @@a ,"), with_match.startToken + 1);
				select_match = search(select_list, parseSearchPattern(", @@a ,"), select_match.startToken + 1);
			}

			sql = sql.substring(0, cte_match.start)
					+ cte_match.variableToValue.get("@@p")
					+ cte_match.variableToValue.get("@@a") + " as (select "
					+ replacement_select_list.substring(1, replacement_select_list.length())
					+ " from " + cte_match.variableToValue.get("@@d") + ")"
					+ sql.substring(cte_match.end, sql.length());
			cte_match = search(sql, cte_pattern, cte_match.startToken + 1);
		}
		return sql;
	}

	/**
	 * Checks if an expression contains an aggregate function
	 *
	 * @param select_expr - the expression to check
	 * @return true if the expression contains an aggregate
	 */
	private static boolean bigQueryExprContainsAggregate(String select_expr) {
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(select_expr);
		List<String> aggregates = Arrays.asList("min", "max", "sum", "count", "count_big", "avg", "stdev", "var", "row_number");
		for (int i = 0; i < tokens.size(); ++i) {
			if (aggregates.contains(tokens.get(i).text.toLowerCase()) &&
					i < (tokens.size() - 1) && tokens.get(i+1).text.equals("(")) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Checks if a GROUP BY clause is all column references
	 *
	 * @param group_by - the clause to check
	 * @return true if the clause is all column references
	 */
	private static boolean bigQueryGroupByIsOnlyColumnReferences(String group_by) {
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(group_by);
		for (int i = 0; i < tokens.size(); ++i) {
			switch (i % 4) {
				case 0:
					break;
				case 1:
					if (!tokens.get(i).text.equals(".")) return false;
					break;
				case 2:
					break;
				case 3:
					if (!tokens.get(i).text.equals(",")) return false;
					break;
			}
		}
		return true;
	}

	/**
	 * Finds the start token for matching if the next match should not overlap the previous match
	 *
	 * @param match - the last match
	 * @param expr - the string that match was found in
	 * @return the start token to pass to search()
	 */
	private static int nextNonoverlappingStartToken(MatchedPattern match, String expr) {
		return match.startToken + StringUtils.tokenizeSql(expr.substring(match.start, match.end)).size() - 1;
	}

	/**
	 * Finds complex expressions in the GROUP BY and replaces them with references to matching select list expressions.
	 *
	 * @param sql - the query to transform
	 * @param select_pattern - the pattern to use to find the SELECTs
	 * @param terminator - the terminating character for the select pattern
	 * @return the query with GROUP BY elements replaced
	 */
	private static String bigQueryReplaceGroupByWithPattern(String sql, String select_pattern, String terminator) {
		List<Block> list_item_pattern = parseSearchPattern(", @@a," );
		List<Block> select_statement_pattern = parseSearchPattern(select_pattern);

		// Iterates SELECT statements
		for (MatchedPattern select_statement_match = search(sql, select_statement_pattern, 0);
			 select_statement_match.start != -1;
			 select_statement_match = search(sql, select_statement_pattern, select_statement_match.startToken + 1)) {
			final String select_list = "," + select_statement_match.variableToValue.get("@@a") + ",";

			// Skips replacing the GROUP BY if it is all column references.
			final String group_by = select_statement_match.variableToValue.get("@@c");
			if (bigQueryGroupByIsOnlyColumnReferences(group_by)) {
				continue;
			}

			String replacement_group_by = "";

			// Iterates the SELECT list and checks for aggregate functions
			MatchedPattern select_expr_match = search(select_list, list_item_pattern, 0);
			for (int i = 1; select_expr_match.start != -1; ++i) {
				final String select_expr = select_expr_match.variableToValue.get("@@a");
				if (!bigQueryExprContainsAggregate(select_expr)) {
					replacement_group_by = replacement_group_by + "," + i;
				}
				select_expr_match = search(select_list, list_item_pattern, nextNonoverlappingStartToken(select_expr_match, select_list));
			}
			sql = sql.substring(0, select_statement_match.start)
					+ "select " + select_statement_match.variableToValue.get("@@a")
					+ " from " + select_statement_match.variableToValue.get("@@b")
					+ " group by " + replacement_group_by.substring(1) + terminator
					+ sql.substring(select_statement_match.end, sql.length());
		}
		return sql;
	}

	/**
	 * Replaces the GROUP BY clause with index references to the SELECT list
	 *
	 * @param sql - the query to transform
	 * @return the query with GROUP BY elements replaced
	 */
	private static String bigQueryReplaceGroupBy(String sql) {
		sql = bigQueryReplaceGroupByWithPattern(sql, "select @@a from @@b group by @@c;", ";");
		sql = bigQueryReplaceGroupByWithPattern(sql, "select @@a from @@b group by @@c)", ")");
		return sql;
	}

	/**
	 * Decides if a select list expression is a string concatenation.
	 *
	 * @param select_expr - the expression to evaluate
	 * @return true if the expression is a string concatenation
	 */
	private static boolean bigQueryExprIsStringConcat(String select_expr) {
		// Additional patterns can be added to this list to heuristically recognize string concatenation expressions
		final String[] concat_exprs = {
				"^ '@@a' +",
				"^ cast(@@a as varchar) +",
				"^ @@a + '@@b'",
				"^ @@a + cast(@@b as varchar)"};
		for (int i=0; i < concat_exprs.length; ++i) {
			MatchedPattern concat_match = search("^" + select_expr, parseSearchPattern(concat_exprs[i]), 0);
			if (concat_match.start != -1) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Converts a string concatenation expression to use CONCAT() rather than +
	 *
	 * @param select_expr - the expression to transform
	 * @return the transformed expression
	 */
	private static String bigQueryReplaceStringConcatsInExpr(String select_expr) {
		select_expr = "+" + select_expr + "+";

		// Finds the AS alias at the end of the expression
		String alias = "";
		MatchedPattern alias_match = search(select_expr, parseSearchPattern("+ @@a as @@b +"), 0);
		if (alias_match.start != -1) {
			select_expr = "+" + alias_match.variableToValue.get("@@a") + "+";
			alias = " as " + alias_match.variableToValue.get("@@b");
		}

		// Repeatedly replace + with CONCAT until there are none left
		List<Block> concat_pattern = parseSearchPattern("+ @@a + @@b +");
		while (true) {
			MatchedPattern concat_match = search(select_expr, concat_pattern, 0);
			if (concat_match.start == -1) {
				break;
			}
			String suffix = "";
			if (concat_match.end < select_expr.length()) {
				suffix = select_expr.substring(concat_match.end + 1);
			}
			select_expr = "+concat(" + concat_match.variableToValue.get("@@a")
					+ ", " + concat_match.variableToValue.get("@@b") + ")+" + suffix;
		}
		return select_expr.substring(1, select_expr.length() - 1) + alias;
	}

	/**
	 * Converts string concatenations using the + operator to CONCAT() calls.
	 *
	 * @param sql - the query to transform
	 * @return the query with string concatenations converted
	 */
	private static String bigQueryReplaceStringConcatsInStatement(String sql) {
		List<Block> list_item_pattern = parseSearchPattern(", @@a," );
		List<Block> select_statement_pattern = parseSearchPattern("select @@a from");

		// Iterates SELECT statements
		for (MatchedPattern select_statement_match = search(sql, select_statement_pattern, 0);
			 select_statement_match.start != -1;
			 select_statement_match = search(sql, select_statement_pattern, select_statement_match.startToken + 1)) {
			final String select_list = "," + select_statement_match.variableToValue.get("@@a") + ",";
			String replacement_select_list = "";

			// Iterates elements of the select list
			for (MatchedPattern select_expr_match = search(select_list, list_item_pattern, 0);
				 select_expr_match.start != -1;
				 select_expr_match = search(select_list, list_item_pattern, nextNonoverlappingStartToken(select_expr_match, select_list))) {
				final String select_expr = select_expr_match.variableToValue.get("@@a");

				// Replace all string concatenations
				String replacement_select_expr = select_expr;
				if (bigQueryExprIsStringConcat(select_expr)) {
					replacement_select_expr = bigQueryReplaceStringConcatsInExpr(replacement_select_expr);
				}

				replacement_select_list = replacement_select_list + "," + replacement_select_expr;
			}
			sql = sql.substring(0, select_statement_match.start)
					+ "select " + replacement_select_list.substring(1) + " from "
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
	 * bigQuery specific translations
	 *
	 * @param sql - the query to translate
	 * @return the query after translation
	 */
	private static String translatebigQuery(String sql) {
		sql = bigQueryLowerCase(sql);
		sql = bigQueryAliasCommonTableExpressions(sql);
		sql = bigQueryReplaceGroupBy(sql);
		sql = bigQueryReplaceStringConcatsInStatement(sql);
		return sql;
	}

	private static String translateSql(String sql, List<String[]> replacementPatterns, String sessionId, String oracleTempPrefix) {
		for (int i = 0; i < replacementPatterns.size(); i++) {
			String[] pair = replacementPatterns.get(i).clone();
			pair[1] = pair[1].replace("%session_id%", sessionId);
			pair[1] = pair[1].replace("%temp_prefix%", oracleTempPrefix);
			List<Block> parsedPattern = parseSearchPattern(pair[0]);
			sql = searchAndReplace(sql, parsedPattern, pair[1]);
		}
		return sql;
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It uses simple pattern replacement, so its functionality is limited.
	 * 
	 * @param sql
	 *            The SQL to be translated
	 * @param sourceDialect
	 *            The source dialect. Currently, only "sql server" for Microsoft SQL Server is supported
	 * @param targetDialect
	 *            The target dialect. Currently "oracle", "postgresql", and "redshift" are supported
	 * @return The translated SQL
	 */
	@Deprecated
	public static String translateSql(String sql, String sourceDialect, String targetDialect) {
		return translateSql(sql, sourceDialect, targetDialect, null, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It uses simple pattern replacement, so its functionality is limited.
	 * 
	 * @param sql
	 *            The SQL to be translated
	 * @param targetDialect
	 *            The target dialect. Currently "oracle", "postgresql", and "redshift" are supported
	 * @return The translated SQL
	 */
	public static String translateSql(String sql, String targetDialect) {
		return translateSql(sql, targetDialect, null, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It uses simple pattern replacement, so its functionality is limited.
	 * 
	 * @param sql
	 *            The SQL to be translated
	 * @param sourceDialect
	 *            The source dialect. Currently, only "sql server" for Microsoft SQL Server is supported
	 * @param targetDialect
	 *            The target dialect. Currently "oracle", "postgresql", and "redshift" are supported
	 * @param sessionId
	 *            An alphanumeric string to be used when generating unique table names (specifically for Oracle temp tables). This ID should preferably be
	 *            generated using the SqlTranslate.generateSessionId() function. If null, a global session ID will be generated and used for all subsequent
	 *            calls to translateSql.
	 * @param oracleTempSchema
	 *            The name of a schema where temp tables can be created in Oracle. When null, the current schema is assumed to be the temp schema (ie. no schema
	 *            name is prefixed to the temp table name).
	 * @return The translated SQL
	 */
	@Deprecated
	public static String translateSql(String sql, String sourceDialect, String targetDialect, String sessionId, String oracleTempSchema) {
		return translateSql(sql, targetDialect, sessionId, oracleTempSchema);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It uses simple pattern replacement, so its functionality is limited.
	 * 
	 * @param sql
	 *            The SQL to be translated
	 * @param SOURCE_DIALECT
	 *            The source dialect. Currently, only "sql server" for Microsoft SQL Server is supported
	 * @param targetDialect
	 *            The target dialect. Currently "oracle", "postgresql", and "redshift" are supported
	 * @param sessionId
	 *            An alphanumeric string to be used when generating unique table names (specifically for Oracle temp tables). This ID should preferably be
	 *            generated using the SqlTranslate.generateSessionId() function. If null, a global session ID will be generated and used for all subsequent
	 *            calls to translateSql.
	 * @param oracleTempSchema
	 *            The name of a schema where temp tables can be created in Oracle. When null, the current schema is assumed to be the temp schema (ie. no schema
	 *            name is prefixed to the temp table name).
	 * @return The translated SQL
	 */
	public static String translateSql(String sql, String targetDialect, String sessionId, String oracleTempSchema) {
		return translateSqlWithPath(sql, targetDialect, sessionId, oracleTempSchema, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It uses simple pattern replacement, so its functionality is limited.
	 * 
	 * @param sql
	 *            The SQL to be translated
	 * @param targetDialect
	 *            The target dialect. Currently "oracle", "postgresql", and "redshift" are supported
	 * @param sessionId
	 *            An alphanumeric string to be used when generating unique table names (specifically for Oracle temp tables). This ID should preferably be
	 *            generated using the SqlTranslate.generateSessionId() function. If null, a global session ID will be generated and used for all subsequent
	 *            calls to translateSql.
	 * @param oracleTempSchema
	 *            The name of a schema where temp tables can be created in Oracle. When null, the current schema is assumed to be the temp schema (ie. no schema
	 *            name is prefixed to the temp table name).
	 * @param pathToReplacementPatterns
	 *            The absolute path of the csv file containing the replacement patterns. If null, the csv file inside the jar is used.
	 * @return The translated SQL
	 */
	public static String translateSqlWithPath(String sql, String targetDialect, String sessionId, String oracleTempSchema, String pathToReplacementPatterns) {
		ensurePatternsAreLoaded(pathToReplacementPatterns);
		if (sessionId == null) {
			if (globalSessionId == null)
				globalSessionId = generateSessionId();
			sessionId = globalSessionId;
		} else
			validateSessionId(sessionId);
		String oracleTempPrefix;
		if (oracleTempSchema == null)
			oracleTempPrefix = "";
		else
			oracleTempPrefix = oracleTempSchema + ".";

		List<String[]> replacementPatterns = targetToReplacementPatterns.get(targetDialect);
		if (replacementPatterns == null) {
			if (SOURCE_DIALECT.equals(targetDialect))
				return sql;
			else {
				Set<String> allowedDialects = new HashSet<String>();
				allowedDialects.add(SOURCE_DIALECT);
				for (String sourceTarget : targetToReplacementPatterns.keySet())
					if (sourceTarget.split("\t")[0].equals(SOURCE_DIALECT))
						allowedDialects.add(sourceTarget.split("\t")[1]);
				throw new RuntimeException("Don't know how to translate from " + SOURCE_DIALECT + " to " + targetDialect + ". Valid target dialects are "
						+ StringUtils.join(allowedDialects, ", "));
			}
		} else
			if (targetDialect.equalsIgnoreCase("bigQuery")) {
				sql = translatebigQuery(sql);
			}
			return translateSql(sql, replacementPatterns, sessionId, oracleTempPrefix);
	}

	private static void validateSessionId(String sessionId) {
		if (sessionId.length() != SESSION_ID_LENGTH)
			throw new RuntimeException("Session ID has length " + sessionId.length() + ", should be " + SESSION_ID_LENGTH);
		if (!Character.isLetter(sessionId.charAt(0)))
			throw new RuntimeException("Session ID does not start with a letter");
		for (int i = 1; i < sessionId.length(); i++)
			if (!Character.isLetterOrDigit(sessionId.charAt(i)))
				throw new RuntimeException("Illegal character in session ID");
	}

	/**
	 * Generates a random string that can be used as a unique session identifier
	 * 
	 * @return
	 */
	public static String generateSessionId() {
		char[] chars = "abcdefghijklmnopqrstuvwxyz0123456789".toCharArray();
		StringBuilder sb = new StringBuilder();
		sb.append(chars[random.nextInt(26)]); // First character has to be letter or Oracle breaks
		for (int i = 1; i < SESSION_ID_LENGTH; i++) {
			char c = chars[random.nextInt(chars.length)];
			sb.append(c);
		}
		return sb.toString();
	}

	private static List<String> line2columns(String line) {
		List<String> columns = StringUtils.safeSplit(line, ',');
		for (int i = 0; i < columns.size(); i++) {
			String column = columns.get(i);
			if (column.startsWith("\"") && column.endsWith("\"") && column.length() > 1)
				column = column.substring(1, column.length() - 1);
			column = column.replace("\\\"", "\"");
			column = column.replace("\\n", "\n");
			columns.set(i, column);
		}
		return columns;
	}

	private static void ensurePatternsAreLoaded(String pathToReplacementPatterns) {
		if (targetToReplacementPatterns != null)
			return;
		else {
			lock.lock();
			if (targetToReplacementPatterns == null) { // Could have been loaded before acquiring the lock
				try {
					InputStream inputStream;
					if (pathToReplacementPatterns == null) // Use CSV file in JAR
						inputStream = SqlTranslate.class.getResourceAsStream("/inst/csv/replacementPatterns.csv");
					else
						inputStream = new FileInputStream(pathToReplacementPatterns);
					BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"));
					targetToReplacementPatterns = new HashMap<String, List<String[]>>();
					String line;
					boolean first = true;
					while ((line = bufferedReader.readLine()) != null) {
						if (first) { // Skip first line
							first = false;
							continue;
						}
						List<String> row = line2columns(line);
						String target = row.get(0);
						List<String[]> replacementPatterns = targetToReplacementPatterns.get(target);
						if (replacementPatterns == null) {
							replacementPatterns = new ArrayList<String[]>();
							targetToReplacementPatterns.put(target, replacementPatterns);
						}
						replacementPatterns.add(new String[] { row.get(1).replaceAll("@", "@@"), row.get(2).replaceAll("@", "@@") });
					}
				} catch (UnsupportedEncodingException e) {
					System.err.println("Computer does not support UTF-8 encoding");
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			lock.unlock();
		}
	}

	public static String[] check(String sql, String targetDialect) {
		List<String> warnings = new ArrayList<String>();

		// temp table names:
		Pattern pattern = Pattern.compile("#[0-9a-zA-Z_]+");
		Matcher matcher = pattern.matcher(sql);
		Set<String> longTempNames = new HashSet<String>();
		while (matcher.find())
			if (matcher.group().length() > MAX_ORACLE_TABLE_NAME_LENGTH - SESSION_ID_LENGTH - 1)
				longTempNames.add(matcher.group());

		for (String longName : longTempNames)
			warnings.add("Temp table name '" + longName + "' is too long. Temp table names should be shorter than "
					+ (MAX_ORACLE_TABLE_NAME_LENGTH - SESSION_ID_LENGTH) + " characters to prevent Oracle from crashing.");

		// normal table names:
		pattern = Pattern.compile("(create|drop|truncate)\\s+table +[0-9a-zA-Z_]+");
		matcher = pattern.matcher(sql.toLowerCase());
		Set<String> longNames = new HashSet<String>();
		while (matcher.find()) {
			String name = sql.substring(matcher.start() + matcher.group().lastIndexOf(" "), matcher.end());
			if (name.length() > MAX_ORACLE_TABLE_NAME_LENGTH && !longTempNames.contains("#" + name))
				longNames.add(name);
		}
		for (String longName : longNames)
			warnings.add("Table name '" + longName + "' is too long. Table names should be shorter than " + MAX_ORACLE_TABLE_NAME_LENGTH
					+ " characters to prevent Oracle from crashing.");

		return warnings.toArray(new String[warnings.size()]);
	}
	
	/**
	 * Forces the replacement patterns to be loaded from the specified path. Useful for debugging.
	 * 
	 * @param pathToReplacementPatterns
	 *            The absolute path of the csv file containing the replacement patterns. If null, the csv file inside the jar is used.
	 */
	public static void setReplacementPatterns(String pathToReplacementPatterns) {
		targetToReplacementPatterns = null;
		ensurePatternsAreLoaded(pathToReplacementPatterns);
	}
}
