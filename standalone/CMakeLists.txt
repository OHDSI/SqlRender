cmake_minimum_required(VERSION 2.6)
project(SqlRender)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -pedantic")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -pedantic")

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS} -g -pg")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS} -g -pg")

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})
set(SQL_RENDER_SOURCE_DIR ${CMAKE_SOURCE_DIR}/../src)
set(STANDALONE_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src)

include_directories(${SQL_RENDER_SOURCE_DIR})

set(SQL_RENDER_SOURCE_FILES
	${SQL_RENDER_SOURCE_DIR}/SqlRender.cpp
	${SQL_RENDER_SOURCE_DIR}/stringUtilities.cpp
	${STANDALONE_SOURCE_DIR}/main.cpp
	)
	
set(SQL_RENDER_TARGET_NAME renderSql)
	
add_executable(${SQL_RENDER_TARGET_NAME} ${SQL_RENDER_SOURCE_FILES})
