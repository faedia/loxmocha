#pragma once

#include "assert_visitor.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/ident_map.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include "gtest/gtest.h"
#include <print>
#include <vector>

namespace loxmocha::test::helpers {

class ident_generator_t {
public:
    [[nodiscard]] auto ident(std::string_view name) -> lexer::token_t
    {
        return lexer::token_t::k_identifier(name, ident_map_.emplace({name.begin(), name.end()}));
    }

    auto map() const -> const lexer::ident_map_t& { return ident_map_; }

private:
    loxmocha::lexer::ident_map_t ident_map_;
};

class ParserTest : public ::testing::Test {
protected:
    template<typename T, typename... Args>
    auto make_vector(Args&&... args) -> std::vector<T>
    {
        std::vector<T> vec;
        vec.reserve(sizeof...(Args));
        (vec.emplace_back("", std::forward<Args>(args)), ...);
        return vec;
    }

    template<typename T, typename... Args>
    auto make_vector2(Args&&... args) -> std::vector<T>
    {
        std::vector<T> vec;
        vec.reserve(sizeof...(Args));
        (vec.emplace_back(std::forward<Args>(args)), ...);
        return vec;
    }

    void base_test(const std::string& source, const auto& expected, auto&& parse)
    {
        lexer::ident_map_t ident_map{};
        // NOLINTNEXTLINE(misc-const-correctness)
        lexer::lexer_t lexer{source, ident_map};
        auto           result = parse(lexer);

        EXPECT_TRUE(!!result) << "Parsing has failed with an error";

        for (const auto& diag : result.diagnostics()) {
            std::print("Diagnostic: {}\n", diag);
        }

        result.result().visit(assert_visitor{ident_map, ident_gen.map()}, expected);
    }

    void decl_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_decl); }

    void expr_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_expr); }

    void stmt_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_stmt); }

    void type_test(const std::string& source, const auto& expected) { base_test(source, expected, parse_type); }

    void rainy_day_test(const std::string& source, const std::vector<std::string>& expected_diagnostics, auto&& parse)
    {
        lexer::ident_map_t ident_map{};
        // NOLINTNEXTLINE(misc-const-correctness)
        lexer::lexer_t lexer{source, ident_map};
        auto           result = parse(lexer);

        EXPECT_FALSE(!!result) << "Parsing was expected to fail but succeeded";

        EXPECT_EQ(result.diagnostics().size(), expected_diagnostics.size())
            << "Number of diagnostics does not match expected";

        for (const auto& [a_diag, e_diag] : std::views::zip(result.diagnostics(), expected_diagnostics)) {
            EXPECT_EQ(a_diag, e_diag) << "Diagnostic message does not match expected. Expected: " << e_diag
                                      << ", Actual: " << a_diag;
        }
    }

    void rainy_day_expr_test(const std::string& source, const std::vector<std::string>& expected_diagnostics)
    {
        rainy_day_test(source, expected_diagnostics, parse_expr);
    }

    // NOLINTNEXTLINE(cppcoreguidelines-non-private-member-variables-in-classes,misc-non-private-member-variables-in-classes)
    ident_generator_t ident_gen;
};

template<typename... Args>
[[nodiscard]] inline auto d(Args&&... decl) -> loxmocha::safe_ptr<loxmocha::ast::decl::decl_t>
{
    return loxmocha::safe_ptr<loxmocha::ast::decl::decl_t>::make("", std::forward<Args>(decl)...);
}

template<typename... Args>
[[nodiscard]] inline auto e(Args&&... expr) -> loxmocha::safe_ptr<loxmocha::ast::expr::expr_t>
{
    return loxmocha::safe_ptr<loxmocha::ast::expr::expr_t>::make("", std::forward<Args>(expr)...);
}

template<typename... Args>
[[nodiscard]] inline auto p(Args&&... pattern) -> loxmocha::safe_ptr<loxmocha::ast::pattern::pattern_t>
{
    return loxmocha::safe_ptr<loxmocha::ast::pattern::pattern_t>::make("", std::forward<Args>(pattern)...);
}

template<typename... Args>
[[nodiscard]] inline auto s(Args&&... stmt) -> loxmocha::safe_ptr<loxmocha::ast::stmt::stmt_t>
{
    return loxmocha::safe_ptr<loxmocha::ast::stmt::stmt_t>::make("", std::forward<Args>(stmt)...);
}

template<typename... Args>
[[nodiscard]] inline auto t(Args&&... args) -> loxmocha::safe_ptr<loxmocha::ast::type::type_t>
{
    return loxmocha::safe_ptr<loxmocha::ast::type::type_t>::make("", std::forward<Args>(args)...);
}

} // namespace loxmocha::test::helpers
