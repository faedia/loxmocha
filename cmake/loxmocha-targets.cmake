include(cmake/loxmocha-warnings.cmake)
include(cmake/loxmocha-clang-tidy.cmake)
include(cmake/loxmocha-cppcheck.cmake)

function(loxmocha_target_common target_name)
    target_compile_features(${target_name} PUBLIC cxx_std_${loxmocha_CXX_STANDARD})
    loxmocha_set_warnings(${target_name})
    loxmocha_add_clang_tidy(${target_name})
    loxmocha_add_cppcheck(${target_name})
endfunction()

function(loxmocha_add_executable name)
    set(target_name loxmocha_${name})
    set(alias_name loxmocha::${name})
    add_executable(${target_name} ${ARGN})
    set_property(TARGET ${target_name} PROPERTY OUTPUT_NAME ${name})
    loxmocha_target_common(${target_name})

    add_executable(${alias_name} ALIAS ${target_name})
endfunction()

function(loxmocha_add_library name)
    set(target_name loxmocha_${name})
    set(alias_name loxmocha::${name})
    add_library(${target_name} ${ARGN})
    loxmocha_target_common(${target_name})


    target_include_directories(
        ${target_name}
        PUBLIC
        \$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )

    add_library(${alias_name} ALIAS ${target_name})
endfunction()

function(loxmocha_add_test name)
    set(target_name loxmocha_${name}_test)
    set(alias_name loxmocha::${name}_test)
    add_executable(${target_name} ${ARGN})
    target_link_libraries(${target_name}
        PRIVATE
        GTest::gtest_main
        loxmocha::${name}
    )
    loxmocha_target_common(${target_name})

    gtest_discover_tests(${target_name})

    add_executable(${alias_name} ALIAS ${target_name})
endfunction()
