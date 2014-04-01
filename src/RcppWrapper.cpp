/**
 * @file RcppWrapper.cpp
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