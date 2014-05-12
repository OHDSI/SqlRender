/**
 * @file SqlSplit.cpp
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

#ifndef __SqlSplit_cpp__
#define __SqlSplit_cpp__

#include "SqlSplit.h"
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

		std::vector<String> SqlSplit::spitSql(String sql) {
			std::vector<String> parts;
			std::vector<Token> tokens = tokenizeSql(toLowerCase(sql));
			int level = 0;
			size_t start = 0;
			size_t cursor;
			for (cursor = start; cursor < tokens.size(); cursor++) {
				Token token = tokens.at(cursor);
				if (token.text == "begin" || token.text == "case") {
					level++;
				} else if (token.text == "end" && (cursor == tokens.size() - 1 || tokens.at(cursor+1).text != "if")) {
					level--;
				} else if (level == 0 && token.text == ";") {
          if (cursor == 0 || tokens.at(cursor - 1).text == "end"){ //oracle: must have ; after end
					  parts.push_back(sql.substr(tokens.at(start).start, token.end - tokens.at(start).start));
          } else { //oracle: cannot have ; after anything but end
            parts.push_back(sql.substr(tokens.at(start).start, token.end - tokens.at(start).start - 1));  
          }
					start = cursor + 1;
				}
			}
			if (start < cursor) {
				parts.push_back(sql.substr(tokens.at(start).start, tokens.at(cursor - 1).end - tokens.at(start).start));
				start = cursor + 1;
			}
			return parts;
		}

	} // namespace sqlRender
} // namespace ohdsi

#endif // __SqlSplit_cpp__
