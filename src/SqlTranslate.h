/**
 * @file SqlTranslate.h
 *
 * This file is part of SqlRender
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

#ifndef __SqlTranslate_h__
#define __SqlTranslate_h__

#include <map>

#include "stringUtilities.h"

namespace ohdsi {
	namespace sqlRender {

		using namespace stringUtilities;

		struct Token {
			size_t start;
			size_t end;
			String text;
		};

		struct Block: Token {
			Block(const Token& other) :
					Token(other), isVariable(false) {
			}
			bool isVariable;
		};

		struct MatchedPattern {
			size_t start;
			size_t end;
			std::map<String, String> variableToValue;

		};

		struct SqlTranslate {
		public:
			typedef stringUtilities::String String;
			typedef std::map<String, String> ReplacementPatterns;

			static String translateSql(String str, ReplacementPatterns& replacementPatterns);

		private:
			static std::vector<Token> tokenize(const String& sql);
			static std::vector<Block> parseSearchPattern(const String& pattern);
			static MatchedPattern search(const String& sql, const std::vector<Block>& parsedPattern, size_t start);
			static String searchAndReplace(const String& sql, const std::vector<Block>& parsedPattern, const String& replacePattern);
		};

	} // namespace sqlRender
} // namespace ohdsi

#endif // __SqlTranslate_h__