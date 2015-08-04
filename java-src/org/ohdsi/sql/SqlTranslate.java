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

public class SqlTranslate {
	public static int							SESSION_ID_LENGTH					= 8;
	private static Map<String, List<String[]>>	sourceTargetToReplacementPatterns	= null;
	private static ReentrantLock				lock								= new ReentrantLock();
	private static Random						random								= new Random();
	private static String						globalSessionId						= null;

	private static class Block extends StringUtils.Token {
		public boolean	isVariable;

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
			if (block.text.length() > 1 && block.text.charAt(0) == '@')
				block.isVariable = true;
			blocks.add(block);
		}
		if (blocks.get(0).isVariable || blocks.get(blocks.size() - 1).isVariable) {
			throw new RuntimeException("Error in search pattern: pattern cannot start or end with a variable: " + pattern);
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
				if (nestStack.size() == 0 && token.text.equals(parsedPattern.get(matchCount + 1).text)) {
					matchedPattern.variableToValue.put(parsedPattern.get(matchCount).text, sql.substring(varStart, token.start));
					matchCount += 2;
					if (matchCount == parsedPattern.size()) {
						matchedPattern.end = token.end;
						return matchedPattern;
					} else if (parsedPattern.get(matchCount).isVariable) {
						varStart = token.end;
					}
					if (token.text.equals("'") || token.text.equals("'"))
						inPatternQuote = !inPatternQuote;
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
						varStart = token.end;
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
	public static String translateSql(String sql, String sourceDialect, String targetDialect) {
		return translateSql(sql, sourceDialect, targetDialect, null, null);
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
	public static String translateSql(String sql, String sourceDialect, String targetDialect, String sessionId, String oracleTempSchema) {
		return translateSql(sql, sourceDialect, targetDialect, sessionId, oracleTempSchema, null);
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
	 * @param pathToReplacementPatterns
	 *            The absolute path of the csv file containing the replacement patterns. If null, the csv file inside the jar is used.
	 * @return The translated SQL
	 */
	public static String translateSql(String sql, String sourceDialect, String targetDialect, String sessionId, String oracleTempSchema,
			String pathToReplacementPatterns) {
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

		List<String[]> replacementPatterns = sourceTargetToReplacementPatterns.get(sourceDialect + "\t" + targetDialect);
		if (replacementPatterns == null) {
			if (sourceDialect.equals(targetDialect))
				return sql;
			else {
				Set<String> allowedDialects = new HashSet<String>();
				allowedDialects.add(sourceDialect);
				for (String sourceTarget : sourceTargetToReplacementPatterns.keySet())
					if (sourceTarget.split("\t")[0].equals(sourceDialect))
						allowedDialects.add(sourceTarget.split("\t")[1]);
				throw new RuntimeException("Don't know how to translate from " + sourceDialect + " to " + targetDialect + ". Valid target dialects are "
						+ StringUtils.join(allowedDialects, ", "));
			}
		} else
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
		if (sourceTargetToReplacementPatterns != null)
			return;
		else {
			lock.lock();
			if (sourceTargetToReplacementPatterns == null) { // Could have been loaded before acquiring the lock
				try {
					InputStream inputStream;
					if (pathToReplacementPatterns == null) // Use CSV file in JAR
						// inputStream = SqlTranslate.class.getResourceAsStream("replacementPatterns.csv");
						inputStream = SqlTranslate.class.getResourceAsStream("/inst/csv/replacementPatterns.csv");
					else
						inputStream = new FileInputStream(pathToReplacementPatterns);
					BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"));
					sourceTargetToReplacementPatterns = new HashMap<String, List<String[]>>();
					String line;
					boolean first = true;
					while ((line = bufferedReader.readLine()) != null) {
						if (first) { // Skip first line
							first = false;
							continue;
						}
						List<String> row = line2columns(line);
						String sourceTarget = row.get(0) + "\t" + row.get(1);
						List<String[]> replacementPatterns = sourceTargetToReplacementPatterns.get(sourceTarget);
						if (replacementPatterns == null) {
							replacementPatterns = new ArrayList<String[]>();
							sourceTargetToReplacementPatterns.put(sourceTarget, replacementPatterns);
						}
						replacementPatterns.add(new String[] { row.get(2), row.get(3) });
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
}
