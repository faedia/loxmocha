loxmocha_add_library(parser
    parser.cpp
    parser-decl.cpp
    parser-expr.cpp
    parser-module.cpp
    parser-pattern.cpp
    parser-stmt.cpp
    parser-type.cpp
)

target_link_libraries(loxmocha_parser
    PUBLIC
    loxmocha::ast
)
