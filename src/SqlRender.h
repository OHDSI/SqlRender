#ifndef __SQLRender_h__
#define __SQLRender_h__

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include "stringUtilities.h"

namespace ohdsi {
	namespace sqlRender {

		using namespace stringUtilities;

		struct Span {
			Span(int _start, int _end) :
					start(_start), end(_end), valid(true) {
			}

			int start;
			int end;
			bool valid;
		};

		struct IfThenElse {

			IfThenElse() :
					condition(NULL), ifTrue(NULL), ifFalse(NULL), hasIfFalse(false) {
			}

			int start() {
				return condition->start;
			}

			int end() {
				return hasIfFalse ? ifFalse->end : ifTrue->end;
			}

			Span* condition;
			Span* ifTrue;
			Span* ifFalse;
			bool hasIfFalse;
		};

		std::ostream& operator<<(std::ostream &strm, const Span &a);

		struct SqlRender { // Just a way to bundle together typedefs, functions
		public:
			typedef stringUtilities::String String; // Defined once in stringUtilities
			typedef stringUtilities::StringVector StringVector;
			typedef std::map<String, String> ParameterMap;
			typedef std::vector<Span> SpanVector;
			typedef std::vector<IfThenElse> ConditionVector;

			static String renderSql(String str, ParameterMap& parameterToValue);

		private:
			static String substituteParameters(const String& str, ParameterMap& parameterToValue);
			static ParameterMap extractDefaults(String& str);
			static SpanVector findCurlyBracketSpans(const String& str);
			static ConditionVector linkIfThenElses(const String &str, SpanVector &spans); // Cannot use const SpanVector, because Span are not const in IfThenElse
			static String parseIfThenElse(const String& str);
			static bool evaluateCondition(const String& str);
			static void replace(String &str, SpanVector &spans, int toReplaceStart, int toReplaceEnd, int replaceWithStart, int replaceWithEnd);
		};

	} // namespace renderer
} // namespace ohdsi

#endif // __SQLRender_h__

