loxmocha_add_test(ast ast-expr-test.cpp)

target_link_libraries(loxmocha_ast_test PRIVATE loxmocha_parser)
