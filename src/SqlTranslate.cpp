/**
 * @file SqlTranslate.cpp
 *
 * This file is part of SQLRender
 *
 * Copyright 2014 Observational Health Data Sciences and Informatics
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @author Observational Health Data Sciences and Informatics
 * @author Martijn Schuemie
 * @author Marc Suchard
 */

#ifndef __SqlTranslate_cpp__
#define __SqlTranslate_cpp__

#include "SqlTranslate.h"

#include <_mingw.h>
#include <R_ext/Error.h>
#include <cctype>
#include <iostream>
#include <stack>
#include <string>
#include <utility>
#include <vector>

//#include "stringUtilities.h"

namespace ohdsi {
	namespace sqlRender {

		std::vector<Block> SqlTranslate::parseSearchPattern(const String& pattern) {
			std::vector<Block> blocks;
			size_t start = 0;
			String lowercasePattern = stringUtilities::toLowerCase(pattern);
			size_t cursor = 0;
			for (; cursor < lowercasePattern.length(); cursor++) {
				char ch = lowercasePattern.at(cursor);
				if (!std::isalnum(ch) && ch != '_' && ch != '@') {
					size_t end = cursor;
					if (ch == '(') {
						end = cursor + 1;
					}
					//if (!(ch == '@' && cursor == start)) {
					if (cursor > start) {
						String content = lowercasePattern.substr(start, end - start);
						content = trim(content);
						if (content.length() > 0) {
							Block block;
							block.content = content;
							block.isVariable = (block.content.at(0) == '@');
							blocks.push_back(block);
							//std::cout << block.content << "\n";
						}
					}
					if (cursor >= end && !std::isspace(ch)) {
						Block block;
						block.content = ch;
						block.isVariable = false;
						blocks.push_back(block);
						//std::cout << block.content << "\n";
						++end;
					}
					start = end;
					//}
				}
			}
			int end = lowercasePattern.length();
			if (cursor != start) {

				if (cursor > start) {
					String content = lowercasePattern.substr(start, end - start);
					content = trim(content);
					if (content.length() > 0) {
						Block block;
						block.content = lowercasePattern.substr(start, end - start);
						block.isVariable = (block.content.at(0) == '@');
						blocks.push_back(block);
						//std::cout << block.content << "\n";
					}
				}
			}
			if (blocks[0].isVariable || blocks[blocks.size() - 1].isVariable) {
				String error("Error in search pattern: pattern cannot start or end with a variable: " + pattern);
				//Todo: throw error
			}

			return blocks;
		}

		MatchedPattern SqlTranslate::search(const String& sql, const std::vector<Block>& parsedPattern, size_t start) {
			String lowercaseSql = toLowerCase(sql);
			size_t blockCursor = 0;
			size_t matchCount = 0;
			size_t varStart = 0;
			//String variableValue;
			std::stack<char> nestStack;
			bool commentType1 = false; //Type 1: -- ... end of line
			bool commentType2 = false; //Type 2: /* .. */
			MatchedPattern matchedPattern;
			for (size_t cursor = start; cursor < lowercaseSql.length(); cursor++) {
				char ch = lowercaseSql.at(cursor);
				if (commentType1) {
					if (ch == '\n')
						commentType1 = false;
				} else if (commentType2) {
					if (ch == '/' && cursor > 0 && lowercaseSql.at(cursor - 1) == '*')
						commentType2 = false;
				} else if (ch == '-' && cursor < lowercaseSql.length() && lowercaseSql.at(cursor + 1) == '-') {
					commentType1 = true;
					matchCount = 0;
				} else if (ch == '/' && cursor < lowercaseSql.length() && lowercaseSql.at(cursor + 1) == '*') {
					commentType2 = true;
					matchCount = 0;
				} else if (matchCount == 0 && std::isspace(ch)) {
					//do nothing
				} else if (parsedPattern[blockCursor].isVariable) {
					if (nestStack.size() == 0 && ch == parsedPattern[blockCursor + 1].content.at(matchCount)) {
						matchCount++;
						if (matchCount == parsedPattern[blockCursor + 1].content.length()) {
							matchedPattern.variableToValue[parsedPattern[blockCursor].content] = sql.substr(varStart, cursor + 1 - matchCount - varStart);
							//std::cout << "'" << parsedPattern[blockCursor].content << "' '" << sql.substr(varStart, cursor + 1 - matchCount - varStart) << "'\n";
							matchCount = 0;
							blockCursor += 2;
							if (blockCursor == parsedPattern.size()) {
								if (cursor == lowercaseSql.length() - 1 || (!std::isalnum(ch) || !std::isalnum(lowercaseSql.at(cursor + 1)))) {
									matchedPattern.end = cursor + 1;
									return matchedPattern;
								} else { //Pattern not allowed to end in the middle of a word
									matchCount = 0;
									blockCursor -= 2;
								}
							}
							if (parsedPattern[blockCursor].isVariable) {
								varStart = cursor + 1;
							}
						}
					} else {
						matchCount = 0;

						if (nestStack.size() != 0 && (nestStack.top() == '"' || nestStack.top() == '\'')) { //inside quoted string
							if (ch == nestStack.top())
								nestStack.pop();
						} else {
							if (ch == '"' || ch == '\'') {
								nestStack.push(ch);
							} else if (ch == '(') {
								nestStack.push(ch);
							} else if (nestStack.size() != 0 && ch == ')' && nestStack.top() == '(') {
								nestStack.pop();
							}
						}
					}
				} else {
					if (ch == parsedPattern[blockCursor].content.at(matchCount)) {
						if (matchCount == 0 && blockCursor == 0) {
							if (cursor == 0 || (!std::isalnum(ch) || !std::isalnum(lowercaseSql.at(cursor - 1)))) { //Pattern not allow to start in the middle of a word
								matchedPattern.start = cursor;
								matchCount++;
							}
						} else {
							matchCount++;
						}
						if (matchCount == parsedPattern[blockCursor].content.length()) {
							matchCount = 0;
							blockCursor++;
							if (blockCursor == parsedPattern.size()) {
								if (cursor == lowercaseSql.length() - 1 || (!std::isalnum(ch) || !std::isalnum(lowercaseSql.at(cursor + 1)))) {
									matchedPattern.end = cursor + 1;
									return matchedPattern;
								} else { //Pattern not allowed to end in the middle of a word
									matchCount = 0;
									blockCursor--;
								}
							} else {
								if (parsedPattern[blockCursor].isVariable) {
									varStart = cursor + 1;
								}
							}
						}
					} else {
						matchCount = 0;
					}
				}
			}
			matchedPattern.start = std::string::npos;
			return matchedPattern;
		}

		String SqlTranslate::searchAndReplace(const String& sql, const std::vector<Block>& parsedPattern, const String& replacePattern) {
			String result(sql);
			size_t start = 0;
			while (start < result.length()) {
				MatchedPattern matchedPattern = search(result, parsedPattern, start);
				if (matchedPattern.start == std::string::npos) {
					start = result.length();
				} else {
					String replacement(replacePattern);
					for (std::map<String, String>::iterator pair = matchedPattern.variableToValue.begin(); pair != matchedPattern.variableToValue.end();
							++pair) {
						replacement = replaceAll(replacement, (*pair).first, (*pair).second);
					}
					result = result.substr(0, matchedPattern.start) + replacement + result.substr(matchedPattern.end, result.length() - matchedPattern.end);
					start = matchedPattern.start;
				}
			}
			return result;
		}

		String SqlTranslate::translateSql(String str, ReplacementPatterns& replacementPatterns) {
			String result(str);

			for (ReplacementPatterns::iterator pair = replacementPatterns.begin(); pair != replacementPatterns.end(); ++pair) {
				std::vector<Block> parsedPattern = parseSearchPattern((*pair).first);
				result = searchAndReplace(result, parsedPattern, (*pair).second);
			}

			return result;
		}

	} // namespace sqlRender
} // namespace ohdsi

#endif // __SqlTranslate_cpp__
