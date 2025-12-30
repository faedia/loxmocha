cmake_minimum_required(VERSION 3.21)

project(
    loxmocha
    VERSION 0.1.0
    DESCRIPTION "LoxMocha Programming Language"
    LANGUAGES CXX
)

set(loxmocha_CXX_STANDARD 23 CACHE STRING "C++ standard for LoxMocha Language")

set(loxmocha_ENABLE_CLANG_TIDY OFF CACHE BOOL "Enable clang-tidy analysis")
set(loxmocha_ENABLE_CPPCHECK OFF CACHE BOOL "Enable cppcheck analysis")

include(FetchContent)

include(cmake/loxmocha-targets.cmake)

add_subdirectory(src)

if(NOT PROJECT_IS_TOP_LEVEL)
    return()
endif()

include(CTest)

add_subdirectory(test)

