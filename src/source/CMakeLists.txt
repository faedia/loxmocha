loxmocha_add_library(source
    source.cpp
)

target_link_libraries(loxmocha_source
    PUBLIC
    loxmocha::memory
)
