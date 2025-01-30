/*******************************************************************************
 * Copyright 2025 Observational Health Data Sciences and Informatics
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SqlTranslate {
	public static int SESSION_ID_LENGTH = 8;
	public static int MAX_TABLE_NAME_LENGTH = 63; // PostreSQL default limit
	private static Map<String, List<String[]>> targetToReplacementPatterns = null;
	private static ReentrantLock lock = new ReentrantLock();
	private static Random random = new Random();
	private static String globalSessionId = null;
	private static String BIG_QUERY = "bigquery";
	private static String IMPALA = "impala";
	private static String SPARK = "spark";

	protected static class Block extends StringUtils.Token {
		public boolean isVariable;
		public String regEx;

		public Block(StringUtils.Token other) {
			super(other);
			isVariable = false;
		}
	}

	protected static class MatchedPattern {
		public int start;
		public int end;
		public int startToken;
		public Map<String, String> variableToValue = new HashMap<String, String>();
	}

	protected static List<Block> parseSearchPattern(String pattern) {
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
					if (escape)
						escape = false;
					else if (tokens.get(j).text.equals("\\"))
						escape = true;
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
			throw new RuntimeException(
					"Error in search pattern: pattern cannot start or end with a non-regex variable: " + pattern);
		}
		return blocks;
	}

	protected static MatchedPattern search(String sql, List<Block> parsedPattern, int startToken) {
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
				if (parsedPattern.get(matchCount).regEx != null
						&& (matchCount == parsedPattern.size() - 1 || parsedPattern.get(matchCount + 1).isVariable)) {
					// Regex variable at end of pattern, or has another variable following it
					Pattern pattern = Pattern.compile(parsedPattern.get(matchCount).regEx,
							Pattern.CASE_INSENSITIVE | Pattern.DOTALL | Pattern.MULTILINE);
					Matcher matcher = pattern.matcher(sql.substring(token.start));
					if (matcher.find() && matcher.start() == 0) {
						if (matchCount == 0) {
							matchedPattern.start = token.start;
							matchedPattern.startToken = cursor;
						}
						matchedPattern.variableToValue.put(parsedPattern.get(matchCount).text,
								sql.substring(token.start, token.start + matcher.end()));
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
						cursor--;
					} else {
						matchCount = 0;
					}
				} else if (nestStack.size() == 0 && matchCount < parsedPattern.size() - 1
						&& token.text.equals(parsedPattern.get(matchCount + 1).text)) {
					// Found the token after the variable
					if (parsedPattern.get(matchCount).regEx != null && matchCount == 0) {
						// First element of pattern is a regEx. Find last part of string prior to subsequent token that matches regEx:
						int start = matchesEnd(parsedPattern.get(matchCount).regEx, sql.substring(varStart, token.start));
						if (start != -1) {
							matchedPattern.variableToValue.put(parsedPattern.get(matchCount).text,
									sql.substring(start + varStart, token.start));
							matchedPattern.start = start + varStart;
							matchedPattern.startToken = cursor;
							matchCount += 2;
							if (matchCount == parsedPattern.size()) {
								matchedPattern.end = token.end;
								return matchedPattern;
							} else if (parsedPattern.get(matchCount).isVariable) {
								varStart = (cursor < tokens.size() - 1) ? tokens.get(cursor + 1).start : -1;
							}
							if (token.text.equals("'") || token.text.equals("'"))
								inPatternQuote = !inPatternQuote;
						}
					} else if (parsedPattern.get(matchCount).regEx != null
							&& !matches(parsedPattern.get(matchCount).regEx, sql.substring(varStart, token.start))) {
						// Content didn't match regex
						matchCount = 0;
						cursor = matchedPattern.startToken;
					} else {
						// No regex or matched regex
						matchedPattern.variableToValue.put(parsedPattern.get(matchCount).text,
								sql.substring(varStart, token.start));
						matchCount += 2;
						if (matchCount == parsedPattern.size()) {
							matchedPattern.end = token.end;
							return matchedPattern;
						} else if (parsedPattern.get(matchCount).isVariable) {
							varStart = (cursor < tokens.size() - 1) ? tokens.get(cursor + 1).start : -1;
						}
						if (token.text.equals("'") || token.text.equals("'"))
							inPatternQuote = !inPatternQuote;
					}
				} else if (matchCount != 0 && nestStack.size() == 0 && !inPatternQuote
						&& (token.text.equals(";") || token.text.equals(")"))) { // Not allowed to span multiple SQL
					// statements or outside of nesting
					matchCount = 0;
					cursor = matchedPattern.startToken;
				} else {
					if (nestStack.size() != 0 && (nestStack.peek().equals("\"") || nestStack.peek().equals("'"))) { // inside
						// quoted
						// string
						if (token.text.equals(nestStack.peek()))
							nestStack.pop();
					} else {
						if (token.text.equals("\"") || token.text.equals("'")) {
							nestStack.push(token.text);
						} else if (!inPatternQuote && token.text.equals("(")) {
							nestStack.push(token.text);
						} else if (!inPatternQuote && nestStack.size() != 0 && token.text.equals(")")
								&& nestStack.peek().equals("(")) {
							nestStack.pop();
						}
					}
				}
			} else {
				// Check if token matches current part of pattern. But first part cannot be
				// within quotes:
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
						varStart = (cursor < tokens.size() - 1) ? tokens.get(cursor + 1).start : -1;
					}
					if (token.text.equals("'") || token.text.equals("\""))
						inPatternQuote = !inPatternQuote;
				} else if (matchCount != 0) {
					matchCount = 0;
					cursor = matchedPattern.startToken;
				}
			}
			if (matchCount != 0 && cursor == tokens.size() - 1) { // If at end of sql and still didn't finish pattern,
				// we're not going to finish it
				matchCount = 0;
				cursor = matchedPattern.startToken;
			}
		}
		matchedPattern.start = -1;
		return matchedPattern;
	}

	private static boolean matches(String regex, String string) {
		Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE | Pattern.DOTALL | Pattern.MULTILINE);
		Matcher matcher = pattern.matcher(string);
		return (matcher.matches());
	}

	private static int matchesEnd(String regex, String string) {
		string = string.replaceAll("\\s$", "");
		Pattern pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE | Pattern.DOTALL | Pattern.MULTILINE);
		Matcher matcher = pattern.matcher(string);
		int start = -1;
		while (matcher.find()) {
			if (matcher.end() == string.length())
				start = matcher.start();
		}
		return start;
	}

	private static String searchAndReplace(String sql, List<Block> parsedPattern, String replacePattern) {
		MatchedPattern matchedPattern = search(sql, parsedPattern, 0);
		while (matchedPattern.start != -1) {
			String replacement = replacePattern;
			for (Map.Entry<String, String> pair : matchedPattern.variableToValue.entrySet())
				replacement = StringUtils.replaceAll(replacement, pair.getKey(), pair.getValue());
			sql = sql.substring(0, matchedPattern.start) + replacement
					+ sql.substring(matchedPattern.end, sql.length());
			// System.out.println(sql);
			int delta = 1;
			if (StringUtils.tokenizeSql(replacement).size() == 0)
				delta = 0;
			// Special situation: if replacement pattern starts with variable, and variable
			// content starts with start of search pattern, don't
			// skip first token:
			if (delta > 0 && replacePattern.startsWith("@@")
					&& replacement.toLowerCase().trim().startsWith(parsedPattern.get(0).text))
				delta = 0;
			matchedPattern = search(sql, parsedPattern, matchedPattern.startToken + delta);
		}
		return sql;
	}

	private static String translateSql(String sql, List<String[]> replacementPatterns, String sessionId,
			String oracleTempPrefix) {
		for (int i = 0; i < replacementPatterns.size(); i++) {
			String[] pair = replacementPatterns.get(i).clone();
			pair[1] = pair[1].replace("%session_id%", sessionId);
			pair[1] = pair[1].replace("%temp_prefix%", oracleTempPrefix);
			List<Block> parsedPattern = parseSearchPattern(pair[0]);
			sql = searchAndReplace(sql, parsedPattern, pair[1]).replaceAll("(?m)^[ \t]*\r?\n", ""); // removes blank lines from each sql statement
		}
		return sql.replaceAll("(?m)^[ \t]*\r?\n", ""); // removes blank lines from final sql statement
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited.
	 *
	 * @param sql           The SQL to be translated
	 * @param sourceDialect The source dialect. Currently, only "sql server" for
	 *                      Microsoft SQL Server is supported
	 * @param targetDialect The target dialect. Currently "oracle", "postgresql",
	 *                      and "redshift" are supported
	 * @return The translated SQL
	 */
	@Deprecated
	public static String translateSql(String sql, String sourceDialect, String targetDialect) {
		return translateSql(sql, sourceDialect, targetDialect, null, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited.The source
	 * dialect is always SQL Server. Note that trailing semicolons are not removed
	 * for Oracle, which is required before sending a statement through JDBC. This
	 * will be done by splitSql.
	 *
	 * @param sql           The SQL to be translated
	 * @param targetDialect The target dialect. Currently "oracle", "postgresql",
	 *                      and "redshift" are supported
	 * @return The translated SQL
	 */
	public static String translateSql(String sql, String targetDialect) {
		return translateSql(sql, targetDialect, null, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited.
	 *
	 * @param sql              The SQL to be translated
	 * @param sourceDialect    The source dialect. Currently, only "sql server" for
	 *                         Microsoft SQL Server is supported
	 * @param targetDialect    The target dialect. Currently "oracle", "postgresql",
	 *                         and "redshift" are supported
	 * @param sessionId        An alphanumeric string to be used when generating
	 *                         unique table names (specifically for Oracle temp
	 *                         tables). This ID should preferably be generated using
	 *                         the SqlTranslate.generateSessionId() function. If
	 *                         null, a global session ID will be generated and used
	 *                         for all subsequent calls to translateSql.
	 * @param oracleTempSchema The name of a schema where temp tables can be created
	 *                         in platforms that do not natively support temp
	 *                         tables. When null, the current schema is assumed to
	 *                         be the temp schema (ie. no schema name is prefixed to
	 *                         the temp table name).
	 * @return The translated SQL
	 */
	@Deprecated
	public static String translateSql(String sql, String sourceDialect, String targetDialect, String sessionId,
			String oracleTempSchema) {
		return translateSql(sql, targetDialect, sessionId, oracleTempSchema);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited. The source
	 * dialect is always SQL Server. Note that trailing semicolons are not removed
	 * for Oracle, which is required before sending a statement through JDBC. This
	 * will be done by splitSql.
	 *
	 * @param sql                 The SQL to be translated
	 * @param targetDialect       The target dialect. Currently "oracle",
	 *                            "postgresql", and "redshift" are supported
	 * @param sessionId           An alphanumeric string to be used when generating
	 *                            unique table names (specifically for Oracle temp
	 *                            tables). This ID should preferably be generated
	 *                            using the SqlTranslate.generateSessionId()
	 *                            function. If null, a global session ID will be
	 *                            generated and used for all subsequent calls to
	 *                            translateSql.
	 * @param tempEmulationSchema The name of a schema where temp tables can be
	 *                            created in Oracle. When null, the current schema
	 *                            is assumed to be the temp schema (ie. no schema
	 *                            name is prefixed to the temp table name).
	 * @return The translated SQL
	 */
	public static String translateSql(String sql, String targetDialect, String sessionId, String tempEmulationSchema) {
		return translateSqlWithPath(sql, targetDialect, sessionId, tempEmulationSchema, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited. The source
	 * dialect is always SQL Server. This removes any trailing semicolon as required
	 * by Oracle when sending through JDBC. A runtime exception is thrown if more
	 * than one statement is encountered in the SQL.
	 *
	 * @param sql           The SQL to be translated
	 * @param targetDialect The target dialect. Currently "oracle", "postgresql",
	 *                      and "redshift" are supported
	 * @return The translated SQL
	 */
	public static String translateSingleStatementSql(String sql, String targetDialect) {
		return translateSingleStatementSql(sql, targetDialect, null, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited.The source
	 * dialect is always SQL Server. This removes any trailing semicolon as required
	 * by Oracle when sending through JDBC. A runtime exception is thrown if more
	 * than one statement is encountered in the SQL.
	 *
	 * @param sql                 The SQL to be translated
	 * @param targetDialect       The target dialect. Currently "oracle",
	 *                            "postgresql", and "redshift" are supported
	 * @param sessionId           An alphanumeric string to be used when generating
	 *                            unique table names (specifically for Oracle temp
	 *                            tables). This ID should preferably be generated
	 *                            using the SqlTranslate.generateSessionId()
	 *                            function. If null, a global session ID will be
	 *                            generated and used for all subsequent calls to
	 *                            translateSql.
	 * @param tempEmulationSchema The name of a schema where temp tables can be
	 *                            created for those platforms that don't support
	 *                            temp tables natively. When null, the current
	 *                            schema is assumed to be the temp schema (ie. no
	 *                            schema name is prefixed to the temp table name).
	 * @return The translated SQL
	 */
	public static String translateSingleStatementSql(String sql, String targetDialect, String sessionId,
			String tempEmulationSchema) {
		return translateSingleStatementSqlWithPath(sql, targetDialect, sessionId, tempEmulationSchema, null);
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited.The source
	 * dialect is always SQL Server. This removes any trailing semicolon as required
	 * by Oracle when sending through JDBC. A runtime exception is thrown if more
	 * than one statement is encountered in the SQL.
	 *
	 * @param sql                       The SQL to be translated
	 * @param targetDialect             The target dialect. Currently "oracle",
	 *                                  "postgresql", and "redshift" are supported
	 * @param sessionId                 An alphanumeric string to be used when
	 *                                  generating unique table names (specifically
	 *                                  for Oracle temp tables). This ID should
	 *                                  preferably be generated using the
	 *                                  SqlTranslate.generateSessionId() function.
	 *                                  If null, a global session ID will be
	 *                                  generated and used for all subsequent calls
	 *                                  to translateSql.
	 * @param tempEmulationSchema       The name of a schema where temp tables can
	 *                                  be created for those platforms that don't
	 *                                  support temp tables natively. When null, the
	 *                                  current schema is assumed to be the temp
	 *                                  schema (ie. no schema name is prefixed to
	 *                                  the temp table name).
	 * @param pathToReplacementPatterns The absolute path of the csv file containing
	 *                                  the replacement patterns. If null, the csv
	 *                                  file inside the jar is used.
	 * @return The translated SQL
	 */
	public static String translateSingleStatementSqlWithPath(String sql, String targetDialect, String sessionId,
			String tempEmulationSchema, String pathToReplacementPatterns) {
		sql = translateSqlWithPath(sql, targetDialect, sessionId, tempEmulationSchema, pathToReplacementPatterns);
		String[] sqlStatements = SqlSplit.splitSql(sql);
		if (sqlStatements.length > 1)
			throw new RuntimeException("SQL contains more than one statement: " + sql);
		return sqlStatements[0];
	}

	/**
	 * This function takes SQL in one dialect and translates it into another. It
	 * uses simple pattern replacement, so its functionality is limited.The source
	 * dialect is always SQL Server.
	 *
	 * @param sql                       The SQL to be translated
	 * @param targetDialect             The target dialect. Currently "oracle",
	 *                                  "postgresql", and "redshift" are supported
	 * @param sessionId                 An alphanumeric string to be used when
	 *                                  generating unique table names (specifically
	 *                                  for Oracle temp tables). This ID should
	 *                                  preferably be generated using the
	 *                                  SqlTranslate.generateSessionId() function.
	 *                                  If null, a global session ID will be
	 *                                  generated and used for all subsequent calls
	 *                                  to translateSql.
	 * @param tempEmulationSchema       The name of a schema where temp tables can
	 *                                  be created for those platforms that don't
	 *                                  support temp tables natively. When null, the
	 *                                  current schema is assumed to be the temp
	 *                                  schema (ie. no schema name is prefixed to
	 *                                  the temp table name).
	 * @param pathToReplacementPatterns The absolute path of the csv file containing
	 *                                  the replacement patterns. If null, the csv
	 *                                  file inside the jar is used.
	 * @return The translated SQL
	 */
	public static String translateSqlWithPath(String sql, String targetDialect, String sessionId,
			String tempEmulationSchema, String pathToReplacementPatterns) {
		ensurePatternsAreLoaded(pathToReplacementPatterns);
		if (sessionId == null) {
			sessionId = getGlobalSessionId();
		} else
			validateSessionId(sessionId);
		String oracleTempPrefix;
		if (tempEmulationSchema == null)
			oracleTempPrefix = "";
		else
			oracleTempPrefix = tempEmulationSchema + ".";

		List<String[]> replacementPatterns = targetToReplacementPatterns.get(targetDialect);
		if (replacementPatterns == null) {
			throw new RuntimeException("Don't know how to translate to " + targetDialect
					+ ". Valid target dialects are " + StringUtils.join(targetToReplacementPatterns.keySet(), ", "));
		} else if (targetDialect.equalsIgnoreCase(BIG_QUERY)) {
			sql = BigQuerySparkTranslate.translatebigQuery(sql);
		} else if (targetDialect.equalsIgnoreCase(SPARK)) {
			sql = BigQuerySparkTranslate.translateSpark(sql);
		}
		sql = translateSql(sql, replacementPatterns, sessionId, oracleTempPrefix);
		if (targetDialect.equalsIgnoreCase(IMPALA) || targetDialect.equalsIgnoreCase(BIG_QUERY) || targetDialect.equals(SPARK)) {
			sql = StringUtils.replaceWithConcat(sql);
		}
		return sql;
	}

	private static void validateSessionId(String sessionId) {
		if (sessionId.length() != SESSION_ID_LENGTH)
			throw new RuntimeException(
					"Session ID has length " + sessionId.length() + ", should be " + SESSION_ID_LENGTH);
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
						replacementPatterns.add(
								new String[] { row.get(1).replaceAll("@", "@@"), row.get(2).replaceAll("@", "@@") });
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
			if (matcher.group().length() > MAX_TABLE_NAME_LENGTH - SESSION_ID_LENGTH - 1)
				longTempNames.add(matcher.group());

		for (String longName : longTempNames)
			warnings.add("Temp table name '" + longName + "' is too long. Temp table names should be shorter than "
					+ (MAX_TABLE_NAME_LENGTH - SESSION_ID_LENGTH)
					+ " characters to prevent some DMBSs from throwing an error.");

		// normal table names:
		pattern = Pattern.compile("(create|drop|truncate)\\s+table +[0-9a-zA-Z_]+");
		matcher = pattern.matcher(sql.toLowerCase());
		Set<String> longNames = new HashSet<String>();
		while (matcher.find()) {
			String name = sql.substring(matcher.start() + matcher.group().lastIndexOf(" "), matcher.end());
			if (name.length() > MAX_TABLE_NAME_LENGTH && !longTempNames.contains("#" + name))
				longNames.add(name);
		}
		for (String longName : longNames)
			warnings.add("Table name '" + longName + "' is too long. Table names should be shorter than "
					+ MAX_TABLE_NAME_LENGTH + " characters to prevent some DMBSs from throwing an error.");

		return warnings.toArray(new String[warnings.size()]);
	}

	/**
	 * Forces the replacement patterns to be loaded from the specified path. Useful
	 * for debugging.
	 *
	 * @param pathToReplacementPatterns The absolute path of the csv file containing
	 *                                  the replacement patterns. If null, the csv
	 *                                  file inside the jar is used.
	 */
	public static void setReplacementPatterns(String pathToReplacementPatterns) {
		targetToReplacementPatterns = null;
		ensurePatternsAreLoaded(pathToReplacementPatterns);
	}

	/**
	 * Get the global session ID. This ID is prefixed to all table names in the temp
	 * emulation schema to avoid collisions between sessions.
	 */
	public static String getGlobalSessionId() {
		if (globalSessionId == null)
			globalSessionId = generateSessionId();
		return(globalSessionId);
	}
}
