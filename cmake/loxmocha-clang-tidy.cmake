find_program(loxmocha_CLANG_TIDY clang-tidy)

if(loxmocha_CLANG_TIDY)
    function(loxmocha_add_clang_tidy target_name)
        set(loxmocha_CLANG_TIDY_OPTIONS
            -extra-arg=-std=c++${loxmocha_CXX_STANDARD}
            -warnings-as-errors=*
        )

        set_target_properties(${target_name} PROPERTIES CXX_CLANG_TIDY "${loxmocha_CLANG_TIDY};${loxmocha_CLANG_TIDY_OPTIONS}")
    endfunction()
else()
    message(FATAL_ERROR "clang-tidy is required to build")
endif()
