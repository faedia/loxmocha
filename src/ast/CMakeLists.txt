loxmocha_add_library(ast decl.cpp expr.cpp module.cpp stmt.cpp)

target_link_libraries(loxmocha_ast
    PUBLIC
    loxmocha::lexer
    loxmocha::memory
    loxmocha::node
)
