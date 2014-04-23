#include <iostream>

#include "stringUtilities.h"
#include "SqlRender.h"
#include "SqlTranslate.h"

//#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::sqlRender;

	String sql = stringUtilities::loadTextFile("c:/temp/test.sql");

	//String sql = "--asdfasdf sdaf \nIF OBJECT_ID('scratch.dbo.self_controlled_cohort_analysis', 'U') IS NOT NULL DROP TABLE scratch.dbo.self_controlled_cohort_analysis;\nIF OBJECT_ID('scratch.dbo.self_controlled_cohort_analysis', 'U') IS NOT NULL DROP TABLE scratch.dbo.self_controlled_cohort_analysis;";
	SqlTranslate::ReplacementPatterns replacementPatterns;
	replacementPatterns["DATEADD(  dd ,@date1,@date2)"] = "(@date1 + @date2)";
	replacementPatterns["DATEDIFF(dd,@start, @end)"] = "(@end - @start)";
	replacementPatterns["IF OBJECT_ID('@table', 'U') IS NOT NULL DROP TABLE @table;"] = "BEGIN\n  EXECUTE IMMEDIATE 'TRUNCATE TABLE @table';\n  EXECUTE IMMEDIATE 'DROP TABLE @table';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;";
	replacementPatterns["USE @schema;"] = "ALTER SESSION SET current_schema = @schema;";
	replacementPatterns[".dbo."] = ".";

	//std::cout << SqlTranslate::translateSql(sql, replacementPatterns);
	sql = SqlTranslate::translateSql(sql, replacementPatterns);
	stringUtilities::saveTextFile(sql, "c:/temp/translatedTest.sql");
	return 0;
}

//#endif
