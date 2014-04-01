#ifndef __stringUtilities_h__   // All files should have if-guards
#define __stringUtilities_h__

#include <string>
#include <vector>

namespace ohdsi {
namespace stringUtilities {

	typedef std::string String; // To enable easy code changes if we decide on a different string/vector implementation	
	typedef std::vector<String> StringVector;

	String& ltrim(String& s); // Do these return references or copies?
	String& rtrim(String& s);
	String& trim(String& s);
// 	String toLowerCase(String& s);
	String toLowerCase(const String& s);
// 	String removeParentheses(String& s);
	String removeParentheses(const String& s);
// 	StringVector multiFindBetween(String& source, const String pre, const String post); 
	StringVector multiFindBetween(const String& source, const String& pre, const String& post); // Do you need copies of pre and post?  No, then use reference
// 	String replaceAll(String source, const String search, const String replace);
	String replaceAll(const String& source, const String& search, const String& replace);
}; // namespace stringUtlities
}; // namespace ohdsi

#endif // __stringUtilities_h__
