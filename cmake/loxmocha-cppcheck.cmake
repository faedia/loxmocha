find_program(loxmocha_CPPCHECK cppcheck)

if(loxmocha_CPPCHECK)
    function(loxmocha_add_cppcheck target_name)
        set(loxmocha_CPPCHECK_OPTIONS
            --enable=warning,style,performance,portability
            --std=c++${loxmocha_CXX_STANDARD}
            --check-level=exhaustive
            --inline-suppr
            --error-exitcode=2
        )
        set_target_properties(${target_name} PROPERTIES CXX_CPPCHECK "${loxmocha_CPPCHECK};${loxmocha_CPPCHECK_OPTIONS}")
    endfunction()
else()
    message(FATAL_ERROR "cppcheck is required to build")
endif()
