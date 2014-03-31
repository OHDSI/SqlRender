#include <string>
#include <vector>

namespace stringUtilities {
std::string &ltrim(std::string &s);
std::string &rtrim(std::string &s);
std::string &trim(std::string &s);
std::string toLowerCase(std::string &s);
std::string removeParentheses(std::string &s);
std::vector<std::string> multiFindBetween(std::string &source, const std::string pre, const std::string post);
std::string replaceAll(std::string source, const std::string search, const std::string replace);
}
;
