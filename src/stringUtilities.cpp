/**
 * @file stringUtilities.cpp
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

#ifndef __stringUtilities_cpp__
#define __stringUtilities_cpp__

#include "stringUtilities.h"

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <functional>
#include <iostream>
#include <iterator>
#include <sstream>
#include <fstream>
#include <string>
#include <cerrno>
#include <iostream>

namespace ohdsi {
	namespace stringUtilities {

		String& ltrim(String& s) {
			s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
			return s;
		}

		String& rtrim(String& s) {
			s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
			return s;
		}

		String& trim(String& s) {
			return ltrim(rtrim(s));
		}

		String toLowerCase(const String& s) {
			String result;
			result.resize(s.size());
			std::transform(s.begin(), s.end(), result.begin(), ::tolower);
			return result;
		}

		String removeParentheses(const String& s) {
			if (s.length() > 1 && ((s.at(0) == '\'' && s.at(s.length() - 1) == '\'') || (s.at(0) == '"' && s.at(s.length() - 1) == '"')))
				return s.substr(1, s.length() - 2);
			else
				return s;
		}

		StringVector multiFindBetween(const String& source, const String& pre, const String& post) { // source, pre and post are not modified, so better to pass reference instead of copy
			typedef std::size_t size_t;

			StringVector values;
			size_t start = 0;
			size_t end = 0;
			while (start != std::string::npos && end != std::string::npos) {
				start = source.find(pre, end);
				if (start != std::string::npos) {
					end = source.find(post, start + pre.length());
					if (end != std::string::npos)
						values.push_back(source.substr(start + pre.length(), end - (start + pre.length())));
				}
			}
			return values;
		}

		String replaceAll(const String& source, const String& search, const String& replace) {
			String result(source);
			size_t pos = 0;
			while ((pos = result.find(search, pos)) != std::string::npos) {
				result.replace(pos, search.length(), replace);
				pos += replace.length();
			}
			return result;
		}

		StringVector split(const String& source, const char delimiter) {
			std::stringstream ss(source);
			StringVector result;
			while (ss.good()) {
				String substr;
				std::getline(ss, substr, delimiter);
				result.push_back(substr);
			}
			return result;
		}

		String loadTextFile(const char *filename) {
			std::ifstream in(filename, std::ios::in | std::ios::binary);
			if (in) {
				std::string contents;
				in.seekg(0, std::ios::end);
				contents.resize(in.tellg());
				in.seekg(0, std::ios::beg);
				in.read(&contents[0], contents.size());
				in.close();
				return (contents);
			}
			throw(errno);
		}

		void saveTextFile(const String& string, const char *filename) {
			std::ofstream myfile(filename);
			if (myfile.is_open()) {
				myfile << string;
				myfile.close();
			} else
				throw(errno);
		}

	} // namespace stringUtilities
} // namespace ohdsi

#endif // __stringUtilities_cpp__
