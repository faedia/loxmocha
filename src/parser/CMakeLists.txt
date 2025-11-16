loxmocha_add_library(parser parser.cpp)

target_link_libraries(loxmocha_parser
    PUBLIC
    loxmocha::ast
)
