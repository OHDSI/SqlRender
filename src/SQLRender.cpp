#include <cstddef>
#include <iostream>
#include <iterator>
#include <map>
#include <stack>
#include <string>
#include <vector>

#include "rcpp.h"
#include "stringUtilities.h"

class Span {
public:
  int start;
	int end;
	bool valid;
	Span(int, int);
};

Span::Span(int start, int end) {
	this->start = start;
	this->end = end;
	valid = true;
}

std::ostream& operator<<(std::ostream &strm, const Span &a) {
	return strm << "(" << a.start << " - " << ")";
}

std::ostream& operator<<(std::ostream &strm, const Span *a) {
	return strm << "(" << a->start << " - " << a->end << ")";
}

class IfThenElse {
public:
	Span* condition;
	Span* ifTrue;
	Span* ifFalse;
	bool hasIfFalse;
	IfThenElse();
	int start();
	int end();
};

IfThenElse::IfThenElse() {
	condition = NULL;
	ifTrue = NULL;
	ifFalse = NULL;
	hasIfFalse = false;
}

int IfThenElse::start() {
	return condition->start;
}

int IfThenElse::end() {
	if (hasIfFalse)
		return ifFalse->end;
	else
		return ifTrue->end;
}

std::ostream& operator<<(std::ostream &strm, const IfThenElse &a) {
	if (a.hasIfFalse)
		return strm << "IF " << a.condition << " THEN " << a.ifTrue << " ELSE " << a.ifFalse;
	else
		return strm << "IF " << a.condition << " THEN " << a.ifTrue;
}

std::vector<Span> findCurlyBracketSpans(std::string &str) {
	std::stack<int> starts;
	std::vector<Span> spans;
	for (unsigned int i = 0; i < str.size(); i++) {
		if (str[i] == '{')
			starts.push(i);
		else if (str[i] == '}') {
			if (!starts.empty()) {
				spans.push_back(Span(starts.top(), i + 1));
				starts.pop();
			}
		}
	}
	return spans;
}

std::vector<IfThenElse> linkIfThenElses(std::string &str, std::vector<Span> &spans) {
	std::vector<IfThenElse> ifThenElses;
	if (spans.size() > 1)
		for (unsigned int i = 0; i < spans.size() - 1; i++)
			for (unsigned int j = i + 1; j < spans.size(); j++) {
				std::string inBetween = str.substr(spans.at(i).end, spans.at(j).start - spans.at(i).end);
				inBetween = stringUtilities::trim(inBetween);
				if (inBetween == "?") {
					IfThenElse ifThenElse;
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

bool evaluateCondition(std::string str) {
	str = stringUtilities::trim(str);
	std::string str_lc = stringUtilities::toLowerCase(str);
	if (str_lc == "false" || str_lc == "0")
		return false;

	std::size_t found = str.find("==");
	if (found != std::string::npos) {
		std::string left = str.substr(0, found);
		left = stringUtilities::trim(left);
		left = stringUtilities::removeParentheses(left);
		std::string right = str.substr(found + 2, str.length());
		right = stringUtilities::trim(right);
		right = stringUtilities::removeParentheses(right);
		return (left == right);
	}
	found = str.find("!=");
	if (found != std::string::npos) {
		std::string left = str.substr(0, found);
		left = stringUtilities::trim(left);
		left = stringUtilities::removeParentheses(left);
		std::string right = str.substr(found + 2, str.length());
		right = stringUtilities::trim(right);
		right = stringUtilities::removeParentheses(right);
		return (left != right);
	}
	return true;
}

void replace(std::string &str, std::vector<Span> &spans, int toReplaceStart, int toReplaceEnd, int replaceWithStart, int replaceWithEnd) {
	std::string replaceWithString = str.substr(replaceWithStart, replaceWithEnd - replaceWithStart + 1);
	str.replace(toReplaceStart, toReplaceEnd - toReplaceStart, replaceWithString);
	for (std::vector<Span>::iterator span = spans.begin(); span != spans.end(); ++span)
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

std::map<std::string, std::string> extractDefaults(std::string &str) {
	//Find all spans containing defaults, and remove them from the main string:
	std::map<std::string, std::string> defaults;
	std::size_t textStart = 0;
	std::size_t defaultStart = 0;
	std::size_t defaultEnd = 0;
	std::string pre = "{DEFAULT ";
	std::string post = "}";
	std::string strippedStr;
	while (defaultStart != std::string::npos && defaultEnd != std::string::npos) {
		defaultStart = str.find(pre, defaultEnd);
		if (defaultStart != std::string::npos) {
			defaultEnd = str.find(post, defaultStart + pre.length());
			if (defaultEnd != std::string::npos) {
				std::string span = str.substr(defaultStart + pre.length(), defaultEnd - (defaultStart + pre.length()));
				std::size_t found = span.find("=");
				if (found != std::string::npos) {
					std::string parameter = span.substr(0, found);
					parameter = stringUtilities::trim(parameter);
					if (parameter.length() > 0 && parameter.at(0) == '@')
						parameter = parameter.substr(1);
					std::string defaultValue = span.substr(found + 2, span.length());
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

std::string substituteParameters(std::string str, std::map<std::string, std::string> &parameterToValue) {
	std::map<std::string, std::string> defaults = extractDefaults(str);
	for (std::map<std::string, std::string>::iterator pair = defaults.begin(); pair != defaults.end(); ++pair)
		if (parameterToValue.find((*pair).first) == parameterToValue.end())
			parameterToValue[(*pair).first] = (*pair).second;

	for (std::map<std::string, std::string>::iterator pair = parameterToValue.begin(); pair != parameterToValue.end(); ++pair)
		str = stringUtilities::replaceAll(str, std::string("@") +(*pair).first, (*pair).second);
	return str;
}

std::string parseIfThenElse(std::string str) {
	std::vector<Span> spans = findCurlyBracketSpans(str);
	std::vector<IfThenElse> ifThenElses = linkIfThenElses(str, spans);
	for (std::vector<IfThenElse>::iterator ifThenElse = ifThenElses.begin(); ifThenElse != ifThenElses.end(); ++ifThenElse) {
		if ((*ifThenElse).condition->valid
				&& evaluateCondition(str.substr((*ifThenElse).condition->start + 1, (*ifThenElse).condition->end - (*ifThenElse).condition->start - 2)))
			replace(str, spans, (*ifThenElse).start(), (*ifThenElse).end(), (*ifThenElse).ifTrue->start + 1, (*ifThenElse).ifTrue->end - 2);
		else {
			if ((*ifThenElse).hasIfFalse)
				replace(str, spans, (*ifThenElse).start(), (*ifThenElse).end(), (*ifThenElse).ifFalse->start + 1, (*ifThenElse).ifFalse->end - 2);
			else
				replace(str, spans, (*ifThenElse).start(), (*ifThenElse).end(), 0, -1); //Don't replace, just remove
		}
	}
	return str;
}

std::string renderSql(std::string str, std::map<std::string, std::string> &parameterToValue) {
	str = substituteParameters(str, parameterToValue);
	str = parseIfThenElse(str);
	return str;
}


// [[Rcpp::export]]
Rcpp::List renderSqlInteral(std::string sql, Rcpp::List parameters) {
  try{
    //Convert list to map:
    std::map<std::string, std::string> parameterToValue;
    Rcpp::List names = parameters.attr("names");
    for (unsigned int i = 0; i < parameters.size(); i++)
      parameterToValue[names[i]] = Rcpp::as<std::string>(parameters[i]);
      
      
    std::string renderedSql = renderSql(sql, parameterToValue);
    return Rcpp::List::create(Rcpp::Named( "parameterizedSql" ) = sql,
                              Rcpp::Named( "sql" ) = renderedSql,
                              Rcpp::Named( "parameters" ) = parameterToValue);

    
  } catch(std::exception &e) {
    forward_exception_to_r(e);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}