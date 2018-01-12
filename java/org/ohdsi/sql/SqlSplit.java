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
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class SqlSplit {

	/**
	 * Splits a string containing multiple SQL statements into a vector of SQL statements
	 * 
	 * @param sql
	 *            The SQL string to split into separate statements
	 * @return An array of strings, one for each SQL statement
	 */
	public static String[] splitSql(String sql) {
		List<String> parts = new ArrayList<String>();
		List<StringUtils.Token> tokens = StringUtils.tokenizeSql(sql.toLowerCase());
		Stack<String> nestStack = new Stack<String>();
		String lastPop = "";
		int start = 0;
		int cursor;
		boolean quote = false;
		boolean bracket = false;
		String quoteText = "";
		for (cursor = start; cursor < tokens.size(); cursor++) {
			StringUtils.Token token = tokens.get(cursor);
			if (quote) {
				if (token.text.equals(quoteText)) {
					quote = false;
				}
			} else if (bracket) {
				if (token.text.equals("]"))
					bracket = false;
			} else if (token.text.equals("'") || token.text.equals("\"")) {
				quote = true;
				quoteText = token.text;
			} else if (token.text.equals("[")) {
					bracket = true;
			} else if (token.text.equals("begin") || token.text.equals("case")) {
				nestStack.push(token.text);
			} else if (token.text.equals("end") && (cursor == tokens.size() - 1 || !tokens.get(cursor + 1).text.equals("if"))) {
				lastPop = nestStack.pop();
			} else if (nestStack.size() == 0 && token.text.equals(";")) {
				if (cursor == 0 || (tokens.get(cursor - 1).text.equals("end") && lastPop.equals("begin"))) { // oracle: must have ; after end belonging to a
																												// begin
					parts.add(sql.substring(tokens.get(start).start, token.end));
				} else { // oracle: cannot have ; after anything but end
					parts.add(sql.substring(tokens.get(start).start, token.end - 1));
				}
				start = cursor + 1;
			}
		}
		if (start < cursor) {
			parts.add(sql.substring(tokens.get(start).start, tokens.get(cursor - 1).end));
			start = cursor + 1;
		}
		return parts.toArray(new String[parts.size()]);
	}
}
