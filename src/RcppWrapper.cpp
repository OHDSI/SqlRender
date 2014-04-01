
#ifndef __RCPPWRAPPER_CPP__
#define __RCPPWRAPPER_CPP__

#include "rcpp.h"
#include "SQLRender.h"

// NB: With this file, SQLRender and friends are now independent of Rcpp, and as such
// can be used in plain C++ projects as well

// [[Rcpp::export]]
Rcpp::List renderSqlInteral(std::string sql, Rcpp::List parameters) {

  using namespace ohdsi::renderer;	

  try {
    //Convert list to map:
    SQLRender::ParameterMap parameterToValue;
    Rcpp::List names = parameters.attr("names");
    for (unsigned int i = 0; i < parameters.size(); i++) {
      parameterToValue[names[i]] = Rcpp::as<std::string>(parameters[i]);
    }      
      
    SQLRender::String renderedSql = SQLRender::renderSql(sql, parameterToValue);
    return Rcpp::List::create(Rcpp::Named( "parameterizedSql" ) = sql,
                              Rcpp::Named( "sql" ) = renderedSql,
                              Rcpp::Named( "parameters" ) = parameterToValue);

    
  } catch(std::exception &e) {
    forward_exception_to_r(e);
  } catch(...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
}

#endif // __RCPPWRAPPER_CPP__