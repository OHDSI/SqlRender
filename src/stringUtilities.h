#ifndef __stringUtilities_h__
#define __stringUtilities_h__

#include <string>
#include <vector>

namespace ohdsi {
	namespace stringUtilities {

		typedef std::string String;
		typedef std::vector<String> StringVector;

		String& ltrim(String& s); // Do these return references or copies?
		String& rtrim(String& s);
		String& trim(String& s);
		String toLowerCase(const String& s);
		String removeParentheses(const String& s);
		StringVector multiFindBetween(const String& source, const String& pre, const String& post);
		String replaceAll(const String& source, const String& search, const String& replace);
	}
	;
// namespace stringUtlities
}
;
// namespace ohdsi

#endif // __stringUtilities_h__
