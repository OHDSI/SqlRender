/**
 * @file stringUtilities.h
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

#ifndef __stringUtilities_h__
#define __stringUtilities_h__

#include <string>
#include <vector>

namespace ohdsi {
	namespace stringUtilities {

		typedef std::string String;
		typedef std::vector<String> StringVector;

		String& ltrim(String& s);
		String& rtrim(String& s);
		String& trim(String& s);
		String toLowerCase(const String& s);
		String removeParentheses(const String& s);
		StringVector multiFindBetween(const String& source, const String& pre, const String& post);
		String replaceAll(const String& source, const String& search, const String& replace);
		StringVector split(const String& source, const char delimiter);
		String loadTextFile(const char *filename);
		void saveTextFile(const String& string, const char *filename);
	} // namespace stringUtlities
} // namespace ohdsi

#endif // __stringUtilities_h__
