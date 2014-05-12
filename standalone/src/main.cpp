#include <iostream>

#include "stringUtilities.h"
#include "SqlRender.h"
#include "SqlTranslate.h"
#include "SqlSplit.h"

//#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::sqlRender;
	std::vector<String> parts = SqlSplit::spitSql("aasdfffd; sdfsadf; begin asadf; asdf; end; ");
	for (size_t i = 0; i < parts.size(); i++) {
		std::cout << parts.at(i) << "\n";
	}

	//String sql = stringUtilities::loadTextFile("c:/temp/test.sql");

//	String sql = "CREATE; TABLE #age_group (\n\n age_group_name VARCHAR(255),\n\n	age_group_min INT,\n\n	age_group_max INT\n\n);";
//	SqlTranslate::ReplacementPatterns replacementPatterns;
//	replacementPatterns["DATEADD(  dd ,@date1,@date2)"] = "(@date1 + @date2)";
//	replacementPatterns["DATEDIFF(dd,@start, @end)"] = "(@end - @start)";
//	replacementPatterns["IF OBJECT_ID('@table', 'U') IS NOT NULL DROP TABLE @table;"] = "BEGIN\n  EXECUTE IMMEDIATE 'TRUNCATE TABLE @table';\n  EXECUTE IMMEDIATE 'DROP TABLE @table';\nEXCEPTION\n  WHEN OTHERS THEN\n    IF SQLCODE != -942 THEN\n      RAISE;\n    END IF;\nEND;";
//	replacementPatterns["USE @schema;"] = "ALTER SESSION SET current_schema = @schema;";
//	replacementPatterns[".dbo."] = ".";
//	replacementPatterns.push_back(std::pair<String,String>("CREATE TABLE #@table (@definition)","CREATE GLOBAL TEMPORARY TABLE @table (@definition) ON COMMIT PRESERVE ROWS"));
//	replacementPatterns.push_back(std::pair<String,String>("#",""));
//	std::cout << SqlTranslate::translateSql(sql, replacementPatterns);
	//sql = SqlTranslate::translateSql(sql, replacementPatterns);
	//stringUtilities::saveTextFile(sql, "c:/temp/translatedTest.sql");
	return 0;
}

//#endif
