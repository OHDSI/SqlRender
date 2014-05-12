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
#include <stdexcept>

namespace ohdsi {
	namespace sqlRender {


		std::vector<Block> SqlTranslate::parseSearchPattern(const String& pattern) {
			std::vector<Token> tokens = tokenizeSql(toLowerCase(pattern));
			std::vector<Block> blocks;
			for (size_t i = 0; i < tokens.size(); i++) {
				Block block(tokens.at(i));
				if (block.text.length() > 1 && block.text.at(0) == '@')
					block.isVariable = true;
				blocks.push_back(block);
			}
			if (blocks[0].isVariable || blocks[blocks.size() - 1].isVariable) {
				String error("Error in search pattern: pattern cannot start or end with a variable: " + pattern);
				throw std::invalid_argument(error);
			}
			return blocks;
		}

		MatchedPattern SqlTranslate::search(const String& sql, const std::vector<Block>& parsedPattern, const int startToken) {
			String lowercaseSql = toLowerCase(sql);
			std::vector<Token> tokens = tokenizeSql(lowercaseSql);
			size_t matchCount = 0;
			size_t varStart = 0;
			std::stack<String> nestStack;
			MatchedPattern matchedPattern;
			for (size_t cursor = startToken; cursor < tokens.size(); cursor++) {
				Token token = tokens.at(cursor);
				if (parsedPattern[matchCount].isVariable) {
					if (nestStack.size() == 0 && token.text == parsedPattern[matchCount + 1].text) {
						matchedPattern.variableToValue[parsedPattern[matchCount].text] = sql.substr(varStart, token.start - varStart);
						//std::cout << parsedPattern[matchCount].text << " = '" <<  sql.substr(varStart, token.start - varStart) << "'\n";
						matchCount += 2;
						if (matchCount == parsedPattern.size()) {
							matchedPattern.end = token.end;
							return matchedPattern;
						} else if (parsedPattern[matchCount].isVariable) {
							varStart = token.end;
						}
					} else if (nestStack.size() == 0 && token.text == ";"){ //Not allowed to span multiple SQL statements
  					matchCount = 0;
					} else {
						if (nestStack.size() != 0 && (nestStack.top() == "\"" || nestStack.top() == "'")) { //inside quoted string
							if (token.text == nestStack.top())
								nestStack.pop();
						} else {
							if (token.text == "\"" || token.text == "'") {
								nestStack.push(token.text);
							} else if (token.text == "(") {
								nestStack.push(token.text);
							} else if (nestStack.size() != 0 && token.text == ")" && nestStack.top() == "(") {
								nestStack.pop();
							}
						}
					}
				} else {
					if (token.text == parsedPattern[matchCount].text) {
						if (matchCount == 0) {
							matchedPattern.start = token.start;
              matchedPattern.startToken = cursor;
						}
						matchCount++;
						if (matchCount == parsedPattern.size()) {
							matchedPattern.end = token.end;
							return matchedPattern;
						} else if (parsedPattern[matchCount].isVariable) {
							varStart = token.end;
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
			MatchedPattern matchedPattern = search(result, parsedPattern, 0);
			while (matchedPattern.start != std::string::npos) {
				String replacement(replacePattern);
				for (std::map<String, String>::iterator pair = matchedPattern.variableToValue.begin(); pair != matchedPattern.variableToValue.end(); ++pair) {
					replacement = replaceAll(replacement, (*pair).first, (*pair).second);
				}
				result = result.substr(0, matchedPattern.start) + replacement + result.substr(matchedPattern.end, result.length() - matchedPattern.end);
				matchedPattern = search(result, parsedPattern, matchedPattern.startToken + 1);
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
