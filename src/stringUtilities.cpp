#include "stringUtilities.h"

#include <algorithm>
#include <cctype>
#include <functional>
#include <iterator>
#include <string>
#include <vector>

namespace stringUtilities {
std::string &ltrim(std::string &s) {
	s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
	return s;
}

std::string &rtrim(std::string &s) {
	s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
	return s;
}

std::string &trim(std::string &s) {
	return stringUtilities::ltrim(stringUtilities::rtrim(s));
}

std::string toLowerCase(std::string &s) {
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
}

std::string removeParentheses(std::string &s) {
	if (s.length() > 1 && ((s.at(0) == '\'' && s.at(s.length() - 1) == '\'') || (s.at(0) == '"' && s.at(s.length() - 1) == '"')))
		return s.substr(1, s.length() - 2);
	else
		return s;
}

std::vector<std::string> multiFindBetween(std::string &source, const std::string pre, const std::string post) {
	std::vector<std::string> values;
	std::size_t start = 0;
	std::size_t end = 0;
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

std::string replaceAll(std::string source, const std::string search, const std::string replace) {
    size_t pos = 0;
    while ((pos = source.find(search, pos)) != std::string::npos) {
         source.replace(pos, search.length(), replace);
         pos += replace.length();
    }
    return source;
}


}
