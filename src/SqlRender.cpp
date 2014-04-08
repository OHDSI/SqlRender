/**
 * @file SqlRender.cpp
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

#ifndef __SqlRender_cpp__
#define __SqlRender_cpp__

#include <cstddef>
#include <iterator>
#include <stack>

#include "stringUtilities.h"
#include "SQLRender.h"

namespace ohdsi {
	namespace sqlRender {

		std::ostream& operator<<(std::ostream &strm, const Span& a) {
			return strm << "(" << a.start << " - " << ")";
		}

		std::ostream& operator<<(std::ostream &strm, const IfThenElse &a) {
			if (a.hasIfFalse) {
				return strm << "IF " << a.condition << " THEN " << a.ifTrue << " ELSE " << a.ifFalse;
			} else {
				return strm << "IF " << a.condition << " THEN " << a.ifTrue;
			}
		}

		SqlRender::SpanVector SqlRender::findCurlyBracketSpans(const String& str) {
			std::stack<int> starts;
			SpanVector spans;
			for (unsigned int i = 0; i < str.size(); i++) {
				if (str[i] == '{') {
					starts.push(i);
				} else if (str[i] == '}') {
					if (!starts.empty()) {
						spans.push_back(Span(starts.top(), i + 1));
						starts.pop();
					}
				}
			}
			return spans;
		}

		SqlRender::SpanVector SqlRender::findParentheses(const String& str) {
			std::stack<int> starts;
			SpanVector spans;
			for (unsigned int i = 0; i < str.size(); i++) {
				if (str[i] == '(') {
					starts.push(i);
				} else if (str[i] == ')') {
					if (!starts.empty()) {
						spans.push_back(Span(starts.top(), i + 1));
						starts.pop();
					}
				}
			}
			return spans;
		}

		SqlRender::ConditionVector SqlRender::linkIfThenElses(const String &str, SpanVector &spans) {
			ConditionVector ifThenElses;
			if (spans.size() > 1)
				for (unsigned int i = 0; i < spans.size() - 1; i++)
					for (unsigned int j = i + 1; j < spans.size(); j++) {
						String inBetween = str.substr(spans.at(i).end, spans.at(j).start - spans.at(i).end);
						inBetween = stringUtilities::trim(inBetween);
						if (inBetween == "?") {
							IfThenElse ifThenElse; // Add constructor that takes these references
							ifThenElse.condition = &spans.at(i);
							ifThenElse.ifTrue = &spans.at(j);
							if (j < spans.size()) {
								for (unsigned int k = j + 1; k < spans.size(); k++) {
									inBetween = str.substr(spans.at(j).end, spans.at(k).start - spans.at(j).end);
									inBetween = stringUtilities::trim(inBetween);
									if (inBetween == ":") {
										ifThenElse.ifFalse = &spans.at(k);
										ifThenElse.hasIfFalse = true;
									}
								}
							}
							ifThenElses.push_back(ifThenElse);
						}
					}
			return ifThenElses;
		}

		bool SqlRender::evaluateCondition(const String& inStr) {
			String str(inStr);
			SpanVector spans = findParentheses(str);
			// Spans are in order of closing parenthesis, so if we go from first to last we'll always process nested parentheses first
			for (SpanVector::iterator span = spans.begin(); span != spans.end(); ++span)
				if (!precededByIn((*span).start, str)) {
					bool evaluation = evaluateBooleanCondition(str.substr((*span).start + 1, (*span).end - (*span).start - 2));
					str.at((*span).start) = evaluation ? '1' : '0';
					replace(str, spans, (*span).start, (*span).end, (*span).start, (*span).start);
				}
			return evaluateBooleanCondition(str);
		}

		bool SqlRender::evaluateBooleanCondition(const String& inStr) {
			String str(inStr);
			str = stringUtilities::trim(str);
			std::size_t found = str.find("&");
			if (found != std::string::npos) {
				StringVector parts = split(str, '&');
				for (StringVector::iterator part = parts.begin(); part != parts.end(); ++part)
					if (!evaluatePrimitiveCondition(*part))
						return false;

				return true;
			}
			found = str.find("|");
			if (found != std::string::npos) {
				StringVector parts = split(str, '|');
				for (StringVector::iterator part = parts.begin(); part != parts.end(); ++part)
					if (evaluatePrimitiveCondition(*part))
						return true;
				return false;
			}
			return evaluatePrimitiveCondition(str);
		}

		bool SqlRender::precededByIn(const int start, const String& inStr) {
			String str(inStr);
			str = stringUtilities::toLowerCase(str);
			int matched = 0;
			for (int i = start - 1; i >= 0; i--) {
				if (!std::isspace(str.at(i))) {
					if (matched == 0 && str.at(i) == 'n')
						matched++;
					else if (matched == 1 && str.at(i) == 'i')
						matched++;
					else
						return false;
				} else if (matched == 2)
					return true;
			}
			return false;
		}

		bool SqlRender::evaluatePrimitiveCondition(const String& inStr) {
			String str(inStr);
			str = stringUtilities::trim(str);
			String str_lc = stringUtilities::toLowerCase(str);
			if (str_lc == "false" || str_lc == "0" || str_lc == "!true" || str_lc == "!1")
				return false;
			if (str_lc == "true" || str_lc == "1" || str_lc == "!false" || str_lc == "!0")
				return true;

			std::size_t found = str.find("==");
			if (found != std::string::npos) {
				String left = str.substr(0, found);
				left = stringUtilities::trim(left);
				left = stringUtilities::removeParentheses(left);
				String right = str.substr(found + 2, str.length());
				right = stringUtilities::trim(right);
				right = stringUtilities::removeParentheses(right);
				return (left == right);
			}
			found = str.find("!=");
			if (found != std::string::npos) {
				String left = str.substr(0, found);
				left = stringUtilities::trim(left);
				left = stringUtilities::removeParentheses(left);
				String right = str.substr(found + 2, str.length());
				right = stringUtilities::trim(right);
				right = stringUtilities::removeParentheses(right);
				return (left != right);
			}
			found = str_lc.find(" in ");
			if (found != std::string::npos) {
				String left = str.substr(0, found);
				left = stringUtilities::trim(left);
				left = stringUtilities::removeParentheses(left);
				String right = str.substr(found + 4, str.length());
				right = stringUtilities::trim(right);
				if (right.length() > 2 && right.at(0) == '(' && right.at(right.length() - 1) == ')') {
					right = right.substr(1, right.length() - 2);
					StringVector parts = split(right, ',');
					for (StringVector::iterator part = parts.begin(); part != parts.end(); ++part) {
						String partString = stringUtilities::removeParentheses((*part));
						if (left == partString)
							return true;
					}
					return false;
				}
			}
			return true;
		}

		void SqlRender::replace(String &str, SpanVector &spans, int toReplaceStart, int toReplaceEnd, int replaceWithStart, int replaceWithEnd) {
			String replaceWithString = str.substr(replaceWithStart, replaceWithEnd - replaceWithStart + 1);
			str.replace(toReplaceStart, toReplaceEnd - toReplaceStart, replaceWithString);
			for (SpanVector::iterator span = spans.begin(); span != spans.end(); ++span)
				if ((*span).valid) {
					if ((*span).start > toReplaceStart) {
						if ((*span).start >= replaceWithStart && (*span).start < replaceWithEnd) {
							int delta = toReplaceStart - replaceWithStart;
							(*span).start += delta;
							(*span).end += delta;
						} else if ((*span).start > toReplaceEnd) {
							int delta = toReplaceStart - toReplaceEnd + replaceWithString.length();
							(*span).start += delta;
							(*span).end += delta;
						} else {
							(*span).valid = false;
						}
					} else if ((*span).end > toReplaceEnd) {
						int delta = toReplaceStart - toReplaceEnd + replaceWithString.length();
						(*span).end += delta;
					}
				}
		}

		SqlRender::ParameterMap SqlRender::extractDefaults(String& str) {
			//Find all spans containing defaults, and remove them from the main string:
			ParameterMap defaults;
			std::size_t textStart = 0;
			std::size_t defaultStart = 0;
			std::size_t defaultEnd = 0;
			String pre = "{DEFAULT ";
			String post = "}";
			String strippedStr;
			while (defaultStart != std::string::npos && defaultEnd != std::string::npos) {
				defaultStart = str.find(pre, defaultEnd);
				if (defaultStart != std::string::npos) {
					defaultEnd = str.find(post, defaultStart + pre.length());
					if (defaultEnd != std::string::npos) {
						String span = str.substr(defaultStart + pre.length(), defaultEnd - (defaultStart + pre.length()));
						std::size_t found = span.find("=");
						if (found != std::string::npos) {
							String parameter = span.substr(0, found);
							parameter = stringUtilities::trim(parameter);
							if (parameter.length() > 0 && parameter.at(0) == '@')
								parameter = parameter.substr(1);
							String defaultValue = span.substr(found + 2, span.length());
							defaultValue = stringUtilities::trim(defaultValue);
							defaultValue = stringUtilities::removeParentheses(defaultValue);
							defaults[parameter] = defaultValue;
						}
						strippedStr.append(str.substr(textStart, defaultStart - textStart));
						textStart = (defaultEnd + post.length());
					}
				}
			}
			strippedStr.append(str.substr(textStart, str.length() - textStart));
			str = strippedStr;
			return defaults;
		}

		String SqlRender::substituteParameters(const String& str, ParameterMap& parameterToValue) {
			String result(str);
			ParameterMap defaults = extractDefaults(result);
			for (ParameterMap::iterator pair = defaults.begin(); pair != defaults.end(); ++pair) { // I love to use {}
				if (parameterToValue.find((*pair).first) == parameterToValue.end()) {
					parameterToValue[(*pair).first] = (*pair).second;
				}
			}

			// How is this not different from a regex expression?
			for (ParameterMap::iterator pair = parameterToValue.begin(); pair != parameterToValue.end(); ++pair) {
				result = stringUtilities::replaceAll(result, String("@") + (*pair).first, (*pair).second);
			}
			return result;
		}

		String SqlRender::parseIfThenElse(const String& str) {
			SpanVector spans = findCurlyBracketSpans(str);
			ConditionVector ifThenElses = linkIfThenElses(str, spans);

			String result(str); // Explicit copy
			for (ConditionVector::iterator ifThenElse = ifThenElses.begin(); ifThenElse != ifThenElses.end(); ++ifThenElse) {
				if ((*ifThenElse).condition->valid) {
					if (evaluateCondition(result.substr((*ifThenElse).condition->start + 1, (*ifThenElse).condition->end - (*ifThenElse).condition->start - 2)))
						replace(result, spans, (*ifThenElse).start(), (*ifThenElse).end(), (*ifThenElse).ifTrue->start + 1, (*ifThenElse).ifTrue->end - 2);
					else {
						if ((*ifThenElse).hasIfFalse)
							replace(result, spans, (*ifThenElse).start(), (*ifThenElse).end(), (*ifThenElse).ifFalse->start + 1,
									(*ifThenElse).ifFalse->end - 2);
						else
							replace(result, spans, (*ifThenElse).start(), (*ifThenElse).end(), 0, -1); //Don't replace, just remove
					}
				}
			}
			return result;
		}

		String SqlRender::renderSql(String str, ParameterMap& parameterToValue) {
			// Should make copy first if you are returning a copy.
			String result = substituteParameters(str, parameterToValue);
			result = parseIfThenElse(result);
			return result;
		}

	} // namespace renderer
} // namespace ohdsi

#endif // __SqlRender_cpp__

