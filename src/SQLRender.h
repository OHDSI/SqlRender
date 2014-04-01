/**
 * @file SQLRender.h
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
 
#ifndef __SQLRender_h__   // All files should have if-guards
#define __SQLRender_h__

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include "stringUtilities.h"

namespace ohdsi { // Put everything in a namespace
namespace renderer { // Nested namespaces are very helpful

using namespace stringUtilities;

struct Span {
	// These structs are sufficient small, where I would inline define functionality to save text space	
	Span(int _start, int _end) : start(_start), end(_end), valid(true) {
		// Use constructor initialization lists to avoid allocation, then copy.		
	}
	
	// Usual to place functions before member objects
	int start;
	int end;
	bool valid;	
};

struct IfThenElse { // struct vs. class, a personal choice based on size, compiler auto-generated functions, etc.
	
	IfThenElse() 
		: condition(NULL), ifTrue(NULL), ifFalse(NULL), hasIfFalse(false) { }
	
	int start() { 
		return condition->start; 
	}
	
	int end() { 
		return hasIfFalse ? ifFalse->end : ifTrue->end; 
	}	 	
	
// 	const Span& condition; // This is never null, so use reference; this is never changed, so use const
// 	const Span& ifTrue; // This is never null so use reference; this is never changed, so use const
	Span* condition;
	Span* ifTrue;
	Span* ifFalse;
	bool hasIfFalse;	
};

std::ostream& operator<<(std::ostream &strm, const Span &a); 

struct SQLRender { // Just a way to bundle together typedefs, functions
public:
	// Here is the public API 
	typedef stringUtilities::String String; // Defined once in stringUtilities
	typedef	stringUtilities::StringVector StringVector;	
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