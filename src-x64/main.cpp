#include <iostream>

#include "SqlRender.h"

#ifdef ECLIPSE

int main() {
	using namespace ohdsi::sqlRender;

	std::string sql = "{true & (true & ())}?{true}:{false}";
	SqlRender::ParameterMap parameters;
	parameters["a"] = "myTable";
	parameters["b"] = "1,2,3";
	std::cout << SqlRender::renderSql(sql, parameters);
	return 0;
}

#endif
