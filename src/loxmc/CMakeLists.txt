loxmocha_add_executable(loxmc main.cpp)

target_link_libraries(loxmocha_loxmc
    PRIVATE
    loxmocha::parser
    loxmocha::source
    CLI11::CLI11
)
