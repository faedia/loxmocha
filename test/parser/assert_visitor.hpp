#pragma once

#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <ranges>

namespace loxmocha::test {
class assert_visitor {
public:
    template<loxmocha::token_t::kind_e... Kinds>
    static auto expect_token_kind(const loxmocha::token_t& token) -> void
    {
        EXPECT_TRUE(((token.kind() == Kinds) || ...))
            << "Expected token kind to be one of the specified kinds, but got: " << static_cast<int>(token.kind());
    }

    void operator()([[maybe_unused]] const auto& actual, [[maybe_unused]] const auto& expected)
    {
        FAIL() << "Unexpected expression type encountered in assertion visitor.";
    }

    void operator()(const auto& actual, const loxmocha::expr::expr_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const loxmocha::expr::literal_t& actual, const loxmocha::expr::literal_t& expected)
    {
        expect_token_kind<loxmocha::token_t::kind_e::k_true,
                          loxmocha::token_t::kind_e::k_false,
                          loxmocha::token_t::kind_e::l_integer,
                          loxmocha::token_t::kind_e::l_char,
                          loxmocha::token_t::kind_e::l_string>(actual.value());

        EXPECT_EQ(actual.value().kind(), expected.value().kind());
        EXPECT_EQ(actual.value().span(), expected.value().span());
    }

    void operator()(const loxmocha::expr::identifier_t& actual, const loxmocha::expr::identifier_t& expected)
    {
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    void operator()(const loxmocha::expr::binary_t& actual, const loxmocha::expr::binary_t& expected)
    {
        EXPECT_EQ(actual.op().kind(), expected.op().kind());
        actual.left()->visit(*this, *expected.left());
        actual.right()->visit(*this, *expected.right());
    }

    void operator()(const loxmocha::expr::unary_t& actual, const loxmocha::expr::unary_t& expected)
    {
        EXPECT_EQ(actual.op().kind(), expected.op().kind());
        actual.operand()->visit(*this, *expected.operand());
    }

    void operator()(const loxmocha::expr::array_t& actual, const loxmocha::expr::array_t& expected)
    {
        ASSERT_EQ(actual.elements().size(), expected.elements().size());

        for (const auto& [a_elem, e_elem] : std::views::zip(actual.elements(), expected.elements())) {
            a_elem.visit(*this, e_elem);
        }
    }

    void operator()(const loxmocha::expr::is_t& actual, const loxmocha::expr::is_t& expected)
    {
        actual.expr()->visit(*this, *expected.expr());
        actual.pattern()->visit(*this, *expected.pattern());
    }

    void operator()(const loxmocha::expr::cast_t& actual, const loxmocha::expr::cast_t& expected)
    {
        actual.expr()->visit(*this, *expected.expr());
        actual.type()->visit(*this, *expected.type());
    }

    void operator()(const loxmocha::expr::tuple_t& actual, const loxmocha::expr::tuple_t& expected)
    {
        ASSERT_EQ(actual.elements().size(), expected.elements().size());

        for (const auto& [a_elem, e_elem] : std::views::zip(actual.elements(), expected.elements())) {
            a_elem.visit(*this, e_elem);
        }
    }

    void operator()(const loxmocha::expr::record_t& actual, const loxmocha::expr::record_t& expected)
    {
        ASSERT_EQ(actual.fields().size(), expected.fields().size());

        for (const auto& [a_field, e_field] : std::views::zip(actual.fields(), expected.fields())) {
            EXPECT_EQ(a_field.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_field.name.kind(), e_field.name.kind());
            EXPECT_EQ(a_field.name.span(), e_field.name.span());
            a_field.value.visit(*this, e_field.value);
        }
    }

    void operator()(const loxmocha::expr::index_t& actual, const loxmocha::expr::index_t& expected)
    {
        actual.base()->visit(*this, *expected.base());
        actual.index()->visit(*this, *expected.index());
    }

    void operator()(const loxmocha::expr::field_t& actual, const loxmocha::expr::field_t& expected)
    {
        EXPECT_EQ(actual.field_name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.field_name().kind(), expected.field_name().kind());
        EXPECT_EQ(actual.field_name().span(), expected.field_name().span());
        actual.base()->visit(*this, *expected.base());
    }

    void operator()(const loxmocha::expr::call_t& actual, const loxmocha::expr::call_t& expected)
    {
        actual.callee()->visit(*this, *expected.callee());

        ASSERT_EQ(actual.positional_args().size(), expected.positional_args().size());

        for (const auto& [a_arg, e_arg] : std::views::zip(actual.positional_args(), expected.positional_args())) {
            a_arg.visit(*this, e_arg);
        }

        ASSERT_EQ(actual.named_args().size(), expected.named_args().size());
        for (const auto& [a_arg, e_arg] : std::views::zip(actual.named_args(), expected.named_args())) {
            EXPECT_EQ(a_arg.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_arg.name.kind(), e_arg.name.kind());
            EXPECT_EQ(a_arg.name.span(), e_arg.name.span());
            a_arg.value.visit(*this, e_arg.value);
        }
    }

    void operator()(const loxmocha::expr::if_t& actual, const loxmocha::expr::if_t& expected)
    {
        ASSERT_EQ(actual.conditional_branches().size(), expected.conditional_branches().size());

        for (const auto& [a_branch, e_branch] :
             std::views::zip(actual.conditional_branches(), expected.conditional_branches())) {
            a_branch.condition->visit(*this, *e_branch.condition);
            a_branch.then_branch->visit(*this, *e_branch.then_branch);
        }

        if (actual.else_branch() && expected.else_branch()) {
            actual.else_branch()->visit(*this, *expected.else_branch());
        } else {
            EXPECT_EQ(!!actual.else_branch(), !!expected.else_branch());
        }
    }

    void operator()(const loxmocha::expr::while_t& actual, const loxmocha::expr::while_t& expected)
    {
        actual.condition()->visit(*this, *expected.condition());
        actual.body()->visit(*this, *expected.body());
    }

    void operator()(const loxmocha::expr::block_t& actual, const loxmocha::expr::block_t& expected)
    {
        ASSERT_EQ(actual.statements().size(), expected.statements().size());

        for (const auto& [a_stmt, e_stmt] : std::views::zip(actual.statements(), expected.statements())) {
            a_stmt.visit(*this, e_stmt);
        }
    }

    void operator()(const loxmocha::expr::grouping_t& actual, const loxmocha::expr::grouping_t& expected)
    {
        actual.expression()->visit(*this, *expected.expression());
    }
};

} // namespace loxmocha::test
