#pragma once

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"

#include "gtest/gtest.h"
#include <cxxabi.h>
#include <memory>
#include <ranges>

namespace loxmocha::test {
class assert_visitor {
public:
    static auto demangle(const char* name) -> std::string
    {
        const std::unique_ptr<char, void (*)(void*)> demangled_ptr(abi::__cxa_demangle(name, nullptr, nullptr, nullptr),
                                                                   std::free);
        std::string                                  result = demangled_ptr ? demangled_ptr.get() : name;
        return result;
    }

    template<loxmocha::token_t::kind_e... Kinds>
    static auto expect_token_kind(const loxmocha::token_t& token) -> void
    {
        EXPECT_TRUE(((token.kind() == Kinds) || ...))
            << "Expected token kind to be one of the specified kinds, but got: " << static_cast<int>(token.kind());
    }

    void operator()([[maybe_unused]] const auto& actual, [[maybe_unused]] const auto& expected)
    {
        FAIL() << "Unexpected expression type encountered in assertion visitor. expected type: "
               << demangle(typeid(expected).name()) << " got type: " << demangle(typeid(actual).name());
    }

    void operator()(const auto& actual, const loxmocha::decl::decl_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const auto& actual, const loxmocha::expr::expr_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const auto& actual, const loxmocha::pattern::pattern_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const auto& actual, const loxmocha::stmt::stmt_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const auto& actual, const loxmocha::type::type_t& expected)
    {
        expected.visit(
            [&visitor = *this](const auto& expected, const auto& actual) -> void { visitor(actual, expected); },
            actual);
    }

    void operator()(const loxmocha::decl::type_t& actual, const loxmocha::decl::type_t& expected)
    {
        EXPECT_EQ(actual.identifier().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.identifier().kind(), expected.identifier().kind());
        EXPECT_EQ(actual.identifier().span(), expected.identifier().span());
        actual.type()->visit(*this, *expected.type());
    }

    void operator()(const loxmocha::decl::function_t& actual, const loxmocha::decl::function_t& expected)
    {
        EXPECT_EQ(actual.identifier().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.identifier().kind(), expected.identifier().kind());
        EXPECT_EQ(actual.identifier().span(), expected.identifier().span());

        ASSERT_EQ(actual.parameters().size(), expected.parameters().size());
        for (const auto& [a_param, e_param] : std::views::zip(actual.parameters(), expected.parameters())) {
            a_param.pattern->visit(*this, *e_param.pattern);
            a_param.type->visit(*this, *e_param.type);
        }

        actual.return_type()->visit(*this, *expected.return_type());
        actual.body()->visit(*this, *expected.body());
    }

    void operator()(const loxmocha::decl::variable_t& actual, const loxmocha::decl::variable_t& expected)
    {
        EXPECT_EQ(actual.identifier().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.identifier().kind(), expected.identifier().kind());
        EXPECT_EQ(actual.identifier().span(), expected.identifier().span());
        actual.type()->visit(*this, *expected.type());
        actual.initialiser()->visit(*this, *expected.initialiser());
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

    void operator()(const loxmocha::pattern::identifier_t& actual, const loxmocha::pattern::identifier_t& expected)
    {
        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    void operator()(const loxmocha::pattern::tag_t& actual, const loxmocha::pattern::tag_t& expected)
    {
        actual.type()->visit(*this, *expected.type());

        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());

        actual.pattern()->visit(*this, *expected.pattern());
    }

    void operator()(const loxmocha::stmt::expr_t& actual, const loxmocha::stmt::expr_t& expected)
    {
        actual.expr()->visit(*this, *expected.expr());
    }

    void operator()(const loxmocha::stmt::assign_t& actual, const loxmocha::stmt::assign_t& expected)
    {
        actual.target()->visit(*this, *expected.target());
        actual.value()->visit(*this, *expected.value());
    }

    void operator()(const loxmocha::stmt::decl_t& actual, const loxmocha::stmt::decl_t& expected)
    {
        actual.declaration()->visit(*this, *expected.declaration());
    }

    void operator()(const loxmocha::type::identifier_t& actual, const loxmocha::type::identifier_t& expected)
    {
        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    void operator()(const loxmocha::type::array_t& actual, const loxmocha::type::array_t& expected)
    {
        actual.element_type()->visit(*this, *expected.element_type());
    }

    void operator()(const loxmocha::type::tuple_t& actual, const loxmocha::type::tuple_t& expected)
    {
        ASSERT_EQ(actual.element_types().size(), expected.element_types().size());

        for (const auto& [a_type, e_type] : std::views::zip(actual.element_types(), expected.element_types())) {
            a_type.visit(*this, e_type);
        }
    }

    void operator()(const loxmocha::type::record_t& actual, const loxmocha::type::record_t& expected)
    {
        ASSERT_EQ(actual.fields().size(), expected.fields().size());

        for (const auto& [a_field, e_field] : std::views::zip(actual.fields(), expected.fields())) {
            EXPECT_EQ(a_field.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_field.name.kind(), e_field.name.kind());
            EXPECT_EQ(a_field.name.span(), e_field.name.span());
            a_field.type.visit(*this, e_field.type);
        }
    }

    void operator()(const loxmocha::type::tagged_t& actual, const loxmocha::type::tagged_t& expected)
    {
        ASSERT_EQ(actual.tags().size(), expected.tags().size());

        for (const auto& [a_tag, e_tag] : std::views::zip(actual.tags(), expected.tags())) {
            EXPECT_EQ(a_tag.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_tag.name.kind(), e_tag.name.kind());
            EXPECT_EQ(a_tag.name.span(), e_tag.name.span());
            a_tag.type.visit(*this, e_tag.type);
        }
    }

    void operator()(const loxmocha::type::reference_t& actual, const loxmocha::type::reference_t& expected)
    {
        actual.base_type()->visit(*this, *expected.base_type());
    }

    void operator()(const loxmocha::type::function_t& actual, const loxmocha::type::function_t& expected)
    {
        ASSERT_EQ(actual.parameters().size(), expected.parameters().size());

        for (const auto& [a_type, e_type] : std::views::zip(actual.parameters(), expected.parameters())) {
            a_type.visit(*this, e_type);
        }

        actual.return_type()->visit(*this, *expected.return_type());
    }

    void operator()(const loxmocha::type::mutable_t& actual, const loxmocha::type::mutable_t& expected)
    {
        actual.base_type()->visit(*this, *expected.base_type());
    }
};

} // namespace loxmocha::test
