

#ifndef __SQLRender_cpp__   // All files should have if-guards
#define __SQLRender_cpp__

#include <cstddef>
#include <iterator>
#include <stack>

#include "rcpp.h"
#include "stringUtilities.h"
#include "SQLRender.h"

namespace ohdsi {
namespace renderer {

using namespace stringUtilities;

std::ostream& operator<<(std::ostream &strm, const Span& a) {
	return strm << "(" << a.start << " - " << ")";
}

// Same as above function
// std::ostream& operator<<(std::ostream &strm, const Span *a) {
// 	return strm << "(" << a->start << " - " << a->end << ")";
// }

std::ostream& operator<<(std::ostream &strm, const IfThenElse &a) {
	if (a.hasIfFalse) {
		return strm << "IF " << a.condition << " THEN " << a.ifTrue << " ELSE " << a.ifFalse;
	} else {
		return strm << "IF " << a.condition << " THEN " << a.ifTrue;
	}
}

SQLRender::SpanVector SQLRender::findCurlyBracketSpans(const String& str) {
	std::stack<int> starts;
	SpanVector spans;
	for (unsigned int i = 0; i < str.size(); i++) {
		if (str[i] == '{') { // use {}s
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

SQLRender::ConditionVector SQLRender::linkIfThenElses(const String &str, SpanVector &spans) {
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

bool SQLRender::evaluateCondition(const String& inStr) {
	String str(inStr);	
	str = stringUtilities::trim(str);
	String str_lc = stringUtilities::toLowerCase(str);
	if (str_lc == "false" || str_lc == "0")
		return false;

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
	return true;
}

void SQLRender::replace(String &str, SpanVector &spans, int toReplaceStart, int toReplaceEnd, int replaceWithStart, int replaceWithEnd) {
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
			}
		}
}

SQLRender::ParameterMap SQLRender::extractDefaults(String& str) {
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

String SQLRender::substituteParameters(const String& str, ParameterMap& parameterToValue) {
	String result(str);
	ParameterMap defaults = extractDefaults(result);
	for (ParameterMap::iterator pair = defaults.begin(); pair != defaults.end(); ++pair) { // I love to use {}
		if (parameterToValue.find((*pair).first) == parameterToValue.end()) {
			parameterToValue[(*pair).first] = (*pair).second;
		}
	}
	
	// How is this not different from a regex expression?
	for (ParameterMap::iterator pair = parameterToValue.begin(); pair != parameterToValue.end(); ++pair) {
		result = stringUtilities::replaceAll(result, String("@") +(*pair).first, (*pair).second);
	}
	return result;
}

String SQLRender::parseIfThenElse(const String& str) {
	SpanVector spans = findCurlyBracketSpans(str);
	ConditionVector ifThenElses = linkIfThenElses(str, spans);
	
	String result(str); // Explicit copy
	for (ConditionVector::iterator ifThenElse = ifThenElses.begin(); ifThenElse != ifThenElses.end(); ++ifThenElse) {
		if ((*ifThenElse).condition->valid
				&& evaluateCondition(result.substr((*ifThenElse).condition->start + 1, (*ifThenElse).condition->end - (*ifThenElse).condition->start - 2)))
			replace(result, spans, (*ifThenElse).start(), (*ifThenElse).end(), (*ifThenElse).ifTrue->start + 1, (*ifThenElse).ifTrue->end - 2);
		else {
			if ((*ifThenElse).hasIfFalse)
				replace(result, spans, (*ifThenElse).start(), (*ifThenElse).end(), (*ifThenElse).ifFalse->start + 1, (*ifThenElse).ifFalse->end - 2);
			else
				replace(result, spans, (*ifThenElse).start(), (*ifThenElse).end(), 0, -1); //Don't replace, just remove
		}
	}
	return result;
}

String SQLRender::renderSql(String str, ParameterMap& parameterToValue) {
	// Should make copy first if you are returning a copy.
	String result = substituteParameters(str, parameterToValue);
	result = parseIfThenElse(result);
	return result;
}


} // namespace renderer
} // namespace ohdsi


// [[Rcpp::export]]
Rcpp::List renderSqlInteral(std::string sql, Rcpp::List parameters) {

	using namespace ohdsi::renderer;	

  try{
    //Convert list to map:
    std::map<std::string, std::string> parameterToValue;
    Rcpp::List names = parameters.attr("names");
    for (unsigned int i = 0; i < parameters.size(); i++) {
      parameterToValue[names[i]] = Rcpp::as<std::string>(parameters[i]);
    }
      
      
    SQLRender::String renderedSql = SQLRender::renderSql(sql, parameterToValue);
    return Rcpp::List::create(Rcpp::Named( "parameterizedSql" ) = sql,
                              Rcpp::Named( "sql" ) = renderedSql,
                              Rcpp::Named( "parameters" ) = parameterToValue);

    
  } catch(std::exception &e) {
    forward_exception_to_r(e);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}

#endif // __SQLRender_cpp__

