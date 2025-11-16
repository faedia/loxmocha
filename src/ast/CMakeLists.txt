loxmocha_add_library(ast expr.cpp module.cpp stmt.cpp)

target_link_libraries(loxmocha_ast
    PUBLIC
    loxmocha::lexer
    loxmocha::memory
)
