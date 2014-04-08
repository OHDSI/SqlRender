#include <iostream>

#include "stringUtilities.h"
#include "SqlRender.h"


#ifdef ECLIPSE

int main() {
	using namespace ohdsi;
	using namespace ohdsi::sqlRender;
	String sql = stringUtilities::loadTextFile("c:/temp/ParamSQLExample3.sql");

	//std::string sql = "{false}?{{true} ? {double true} : {true false}} : {false}";
	SqlRender::ParameterMap parameters;
	parameters["drug_concept_id_list"] = "767410,1314924,907879";
	parameters["condition_concept_id_list"] = "444382, 79106, 138825";
	parameters["stratify_gender"] = "0";
	parameters["stratify_age"] = "0";
	parameters["stratify_index"] = "0";
	parameters["first_occurrence_drug_only"] = "0";
	parameters["first_occurrence_condition_only"] = "0";
	parameters["min_index"] = "";
	parameters["max_index"] = "";


//	std::cout << SqlRender::renderSql(sql, parameters);
	sql = SqlRender::renderSql(sql, parameters);
	stringUtilities::saveTextFile(sql, "c:/temp/renderedSql.sql");
	return 0;
}

#endif
