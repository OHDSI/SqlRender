#include <iostream>

#include "stringUtilities.h"
#include "SqlRender.h"
#include "SqlTranslate.h"

//#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::sqlRender;

	//String sql = stringUtilities::loadTextFile("c:/temp/test.sql");

	String sql = "abc.dbo.def AND abc.dbo.def";
	SqlTranslate::ReplacementPatterns replacementPatterns;
//	replacementPatterns["DATEADD(  dd ,@date1,@date2)"] = "(@date1 + @date2)";
//	replacementPatterns["DATEDIFF(dd,@start, @end)"] = "(@end - @start)";
//	replacementPatterns["IF OBJECT_ID('@table', 'U') IS NOT NULL DROP TABLE @table;"] = "TRUNCATE TABLE @table;\nDROP TABLE @table;";
//	replacementPatterns["USE @schema;"] = "ALTER SESSION SET current_schema = @schema;";
	replacementPatterns[".dbo. /*sdgfdsg*/"] = ".";

	std::cout << SqlTranslate::translateSql(sql, replacementPatterns);
	//sql = SqlTranslate::translateSql(sql, replacementPatterns);
	//stringUtilities::saveTextFile(sql, "c:/temp/translatedTest.sql");
	return 0;
}

//#endif
