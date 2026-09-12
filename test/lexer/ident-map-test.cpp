#include "loxmocha/ast/ident_map.hpp"

#include <gtest/gtest.h>
#include <map>
#include <string>

using namespace loxmocha::lexer;

TEST(IdentMapTest, ManySmallIdentsTest)
{
    loxmocha::lexer::ident_map_t   ident_map{};
    std::map<ident_t, std::string> idents{};

    for (char c1 = 'a'; c1 <= 'z'; ++c1) {
        idents.emplace(ident_map.emplace({c1}), std::string{c1});
        for (char c2 = 'a'; c2 <= 'z'; ++c2) {
            idents.emplace(ident_map.emplace({c1, c2}), std::string{c1, c2});
        }
    }

    for (const auto& [ident, c] : idents) {
        const auto& stored_str = ident_map[ident];
        ASSERT_EQ(stored_str, c);

        const auto& looked_up_ident = ident_map.emplace(std::string(c));
        ASSERT_EQ(looked_up_ident, ident);
    }
}
