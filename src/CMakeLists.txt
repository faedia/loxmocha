FetchContent_Declare(
    CLI11
    GIT_REPOSITORY https://github.com/CLIUtils/CLI11
    GIT_TAG v2.6.1
)

FetchContent_MakeAvailable(CLI11)

add_subdirectory(ast)
add_subdirectory(loxmc)
add_subdirectory(lexer)
add_subdirectory(memory)
add_subdirectory(node)
add_subdirectory(parser)
add_subdirectory(source)
