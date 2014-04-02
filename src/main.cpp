#include <iostream>

#include "SqlRender.h"

#ifdef ECLIPSE

int main() {
	using namespace ohdsi::sqlRender;

	std::string sql = "SELECT * FROM @a {@b != ''}?{WHERE @b = 1};";
	SqlRender::ParameterMap parameters;
	parameters["a"] = "myTable";
	parameters["b"] = "";
	std::cout << SqlRender::renderSql(sql, parameters);
	return 0;

}

#endif
