function(loxmocha_set_warnings target_name)
    set(loxmocha_WARNINGS_CXX
        -Wall
        -Wextra # Baseline warnings.
        -Werror # Treat warnings as errors.
    )

    target_compile_options(${target_name} PUBLIC ${loxmocha_WARNINGS_CXX})

endfunction()
