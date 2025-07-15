loxmocha_add_executable(loxmc main.cpp)

target_link_libraries(loxmocha_loxmc
    PRIVATE
    loxmocha::lexer
)
