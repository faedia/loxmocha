loxmocha_add_test(parser
    parser-decl-test.cpp
    parser-expr-test.cpp
    parser-module-test.cpp
    parser-pattern-test.cpp
    parser-stmt-test.cpp
    parser-type-test.cpp)

target_link_libraries(loxmocha_parser_test PRIVATE loxmocha_parser)
