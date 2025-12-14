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

/**
 * @class assert_visitor
 * @brief This is a helper visitor class for asserting equality between two ASTs in tests.
 *
 * It traverses the actual and expected tree simultaneously, assert that each node matches.
 */
class assert_visitor {
public:
    /**
     * @brief Demangle a type name for better readability in test output.
     * @param name The mangled type name.
     * @return std::string The demangled type name.
     */
    static auto demangle(const char* name) -> std::string
    {
        // use __cxa_demangle to demangle the name given by typeid
        // We save the pointer in a unique_ptr make sure it is freed after we copy it to a string
        const std::unique_ptr<char, void (*)(void*)> demangled_ptr(abi::__cxa_demangle(name, nullptr, nullptr, nullptr),
                                                                   std::free);
        std::string                                  result = demangled_ptr ? demangled_ptr.get() : name;
        return result;
    }

    /**
     * @brief Expect the given token to be one of the specified kinds.
     *
     * @tparam Kinds The kinds to expect.
     * @param token The token to check.
     */
    template<loxmocha::token_t::kind_e... Kinds>
    static auto expect_token_kind(const loxmocha::token_t& token) -> void
    {
        EXPECT_TRUE(((token.kind() == Kinds) || ...))
            << "Expected token kind to be one of the specified kinds, but got: " << static_cast<int>(token.kind());
    }

    /**
     * @brief Base case visitor for when two node types do not match!
     * This fails the test with a message outputting the expected and actual types.
     *
     * @param actual The actual node.
     * @param expected The expected node.
     */
    void operator()([[maybe_unused]] const auto& actual, [[maybe_unused]] const auto& expected)
    {
        FAIL() << "Unexpected expression type encountered in assertion visitor. expected type: "
               << demangle(typeid(expected).name()) << " got type: " << demangle(typeid(actual).name());
    }

    void operator()([[maybe_unused]] const loxmocha::node_base_t& base, const auto& actual, const auto& expected)
    {
        this->operator()(actual, expected);
    }

    /**
     * @brief Unwraps the expected node and then calls back into the visitor with the actual and expected nodes.
     *
     * @param actual The actual node.
     * @param expected The wrapped up expected node.
     */
    void operator()(const auto& actual, const loxmocha::decl::decl_t& expected)
    {
        expected.visit([&visitor = *this]([[maybe_unused]] const node_base_t& base,
                                          const auto&                         expected,
                                          const auto& actual) -> void { visitor(actual, expected); },
                       actual);
    }

    /**
     * @brief Unwraps the expected node and then calls back into the visitor with the actual and expected nodes.
     *
     * @param actual The actual node.
     * @param expected The wrapped up expected node.
     */
    void operator()(const auto& actual, const loxmocha::expr::expr_t& expected)
    {
        expected.visit([&visitor = *this]([[maybe_unused]] const node_base_t& base,
                                          const auto&                         expected,
                                          const auto& actual) -> void { visitor(actual, expected); },
                       actual);
    }

    /**
     * @brief Unwraps the expected node and then calls back into the visitor with the actual and expected nodes.
     *
     * @param actual The actual node.
     * @param expected The wrapped up expected node.
     */
    void operator()(const auto& actual, const loxmocha::pattern::pattern_t& expected)
    {
        expected.visit([&visitor = *this]([[maybe_unused]] const node_base_t& base,
                                          const auto&                         expected,
                                          const auto& actual) -> void { visitor(actual, expected); },
                       actual);
    }

    /**
     * @brief Unwraps the expected node and then calls back into the visitor with the actual and expected nodes.
     *
     * @param actual The actual node.
     * @param expected The wrapped up expected node.
     */
    void operator()(const auto& actual, const loxmocha::stmt::stmt_t& expected)
    {
        expected.visit([&visitor = *this]([[maybe_unused]] const node_base_t& base,
                                          const auto&                         expected,
                                          const auto& actual) -> void { visitor(actual, expected); },
                       actual);
    }

    /**
     * @brief Unwraps the expected node and then calls back into the visitor with the actual and expected nodes.
     *
     * @param actual The actual node.
     * @param expected The wrapped up expected node.
     */
    void operator()(const auto& actual, const loxmocha::type::type_t& expected)
    {
        expected.visit([&visitor = *this]([[maybe_unused]] const node_base_t& base,
                                          const auto&                         expected,
                                          const auto& actual) -> void { visitor(actual, expected); },
                       actual);
    }

    /**
     * @brief Asserts that two type declarations are equal.
     *
     * @param actual The actual type declaration.
     * @param expected The expected type declaration.
     */
    void operator()(const loxmocha::decl::type_t& actual, const loxmocha::decl::type_t& expected)
    {
        // Confirm that the names are the same and are identifiers.
        EXPECT_EQ(actual.identifier().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.identifier().kind(), expected.identifier().kind());
        EXPECT_EQ(actual.identifier().span(), expected.identifier().span());
        // Compare the type expressions of the decl.
        actual.type()->visit(*this, *expected.type());
    }

    /**
     * @brief Asserts that two function declarations are equal.
     *
     * @param actual The actual function declaration.
     * @param expected The expected function declaration.
     */
    void operator()(const loxmocha::decl::function_t& actual, const loxmocha::decl::function_t& expected)
    {
        // Confirm that the names are the same and are identifiers.
        EXPECT_EQ(actual.identifier().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.identifier().kind(), expected.identifier().kind());
        EXPECT_EQ(actual.identifier().span(), expected.identifier().span());

        // Make sure that the parameters are the same size and compare each one.
        ASSERT_EQ(actual.parameters().size(), expected.parameters().size());
        for (const auto& [a_param, e_param] : std::views::zip(actual.parameters(), expected.parameters())) {
            // Confirm that the parameter names are the same and are identifiers.
            EXPECT_EQ(a_param.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_param.name.kind(), e_param.name.kind());
            EXPECT_EQ(a_param.name.span(), e_param.name.span());
            a_param.type.visit(*this, e_param.type);
        }

        // Make sure the return types and bodies are the same.
        actual.return_type()->visit(*this, *expected.return_type());
        actual.body()->visit(*this, *expected.body());
    }

    /**
     * @brief Asserts that two variable declarations are equal.
     *
     * @param actual The actual variable declaration.
     * @param expected The expected variable declaration.
     */
    void operator()(const loxmocha::decl::variable_t& actual, const loxmocha::decl::variable_t& expected)
    {
        // Make sure the variable mutibility, identifier, type and initialiser are the same.
        EXPECT_EQ(actual.mutability(), expected.mutability());
        EXPECT_EQ(actual.identifier().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.identifier().kind(), expected.identifier().kind());
        EXPECT_EQ(actual.identifier().span(), expected.identifier().span());
        actual.type()->visit(*this, *expected.type());
        actual.initialiser()->visit(*this, *expected.initialiser());
    }

    /**
     * @brief Asserts that two literal expressions are equal.
     *
     * @param actual The actual literal expression.
     * @param expected The expected literal expression.
     */
    void operator()(const loxmocha::expr::literal_t& actual, const loxmocha::expr::literal_t& expected)
    {
        // Make sure the value kinds are literal kinds.
        expect_token_kind<loxmocha::token_t::kind_e::k_true,
                          loxmocha::token_t::kind_e::k_false,
                          loxmocha::token_t::kind_e::l_integer,
                          loxmocha::token_t::kind_e::l_char,
                          loxmocha::token_t::kind_e::l_string>(actual.value());

        // Then make sure the value kinds and spans are the same.
        EXPECT_EQ(actual.value().kind(), expected.value().kind());
        EXPECT_EQ(actual.value().span(), expected.value().span());
    }

    /**
     * @brief Asserts that two identifier expressions are equal.
     *
     * @param actual The actual identifier expression.
     * @param expected The expected identifier expression.
     */
    void operator()(const loxmocha::expr::identifier_t& actual, const loxmocha::expr::identifier_t& expected)
    {
        // Make sure the name kinds are identifier kinds.
        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        // Then make sure the name kinds and spans are the same.
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    /**
     * @brief Asserts that two binary expressions are equal.
     *
     * @param actual The actual binary expression.
     * @param expected The expected binary expression.
     */
    void operator()(const loxmocha::expr::binary_t& actual, const loxmocha::expr::binary_t& expected)
    {
        // Make sure the operator kinds are the same.
        EXPECT_EQ(actual.op().kind(), expected.op().kind());
        // Make sure the left and right expressions are the same.
        actual.left()->visit(*this, *expected.left());
        actual.right()->visit(*this, *expected.right());
    }

    /**
     * @brief Asserts that two unary expressions are equal.
     *
     * @param actual The actual unary expression.
     * @param expected The expected unary expression.
     */
    void operator()(const loxmocha::expr::unary_t& actual, const loxmocha::expr::unary_t& expected)
    {
        // Make sure the operator kinds are the same.
        EXPECT_EQ(actual.op().kind(), expected.op().kind());
        // Make sure the operand expressions are the same.
        actual.operand()->visit(*this, *expected.operand());
    }

    /**
     * @brief Asserts that two array expressions are equal.
     *
     * @param actual The actual array expression.
     * @param expected The expected array expression.
     */
    void operator()(const loxmocha::expr::array_t& actual, const loxmocha::expr::array_t& expected)
    {
        // Make sure they have the same number of elements.
        ASSERT_EQ(actual.elements().size(), expected.elements().size());

        // Then make sure each element matches.
        for (const auto& [a_elem, e_elem] : std::views::zip(actual.elements(), expected.elements())) {
            a_elem.visit(*this, e_elem);
        }
    }

    /**
     * @brief Asserts that two is expressions are equal.
     *
     * @param actual The actual is expression.
     * @param expected The expected is expression.
     */
    void operator()(const loxmocha::expr::is_t& actual, const loxmocha::expr::is_t& expected)
    {
        // Make sure the expression and pattern are the same.
        actual.expr()->visit(*this, *expected.expr());
        actual.pattern()->visit(*this, *expected.pattern());
    }

    /**
     * @brief Asserts that two cast expressions are equal.
     *
     * @param actual The actual cast expression.
     * @param expected The expected cast expression.
     */
    void operator()(const loxmocha::expr::cast_t& actual, const loxmocha::expr::cast_t& expected)
    {
        // Make sure the expression and type are the same.
        actual.expr()->visit(*this, *expected.expr());
        actual.type()->visit(*this, *expected.type());
    }

    /**
     * @brief Asserts that two tuple expressions are equal.
     *
     * @param actual The actual tuple expression.
     * @param expected The expected tuple expression.
     */
    void operator()(const loxmocha::expr::tuple_t& actual, const loxmocha::expr::tuple_t& expected)
    {
        // Make sure they have the same number of elements.
        ASSERT_EQ(actual.elements().size(), expected.elements().size());

        // Then make sure each element matches.
        for (const auto& [a_elem, e_elem] : std::views::zip(actual.elements(), expected.elements())) {
            a_elem.visit(*this, e_elem);
        }
    }

    /**
     * @brief Asserts that two record expressions are equal.
     *
     * @param actual The actual record expression.
     * @param expected The expected record expression.
     */
    void operator()(const loxmocha::expr::record_t& actual, const loxmocha::expr::record_t& expected)
    {
        // Make sure they have the same number of fields.
        ASSERT_EQ(actual.fields().size(), expected.fields().size());

        // Then make sure each field matches.
        for (const auto& [a_field, e_field] : std::views::zip(actual.fields(), expected.fields())) {
            // Confirm that the field names are the same and are identifiers.
            EXPECT_EQ(a_field.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_field.name.kind(), e_field.name.kind());
            EXPECT_EQ(a_field.name.span(), e_field.name.span());
            // Then compare the field values.
            a_field.value.visit(*this, e_field.value);
        }
    }

    /**
     * @brief Asserts that two index expressions are equal.
     *
     * @param actual The actual index expression.
     * @param expected The expected index expression.
     */
    void operator()(const loxmocha::expr::index_t& actual, const loxmocha::expr::index_t& expected)
    {
        // Make sure the base and index expressions are the same.
        actual.base()->visit(*this, *expected.base());
        actual.index()->visit(*this, *expected.index());
    }

    /**
     * @brief Asserts that two field expressions are equal.
     *
     * @param actual The actual field expression.
     * @param expected The expected field expression.
     */
    void operator()(const loxmocha::expr::field_t& actual, const loxmocha::expr::field_t& expected)
    {
        // Confirm that the field names are the same and are identifiers.
        EXPECT_EQ(actual.field_name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.field_name().kind(), expected.field_name().kind());
        EXPECT_EQ(actual.field_name().span(), expected.field_name().span());
        // Then compare the base expressions.
        actual.base()->visit(*this, *expected.base());
    }

    /**
     * @brief Asserts that two call expressions are equal.
     *
     * @param actual The actual call expression.
     * @param expected The expected call expression.
     */
    void operator()(const loxmocha::expr::call_t& actual, const loxmocha::expr::call_t& expected)
    {
        // Make sure the callee exprsessions match
        actual.callee()->visit(*this, *expected.callee());

        // Make sure the number of positional args match
        ASSERT_EQ(actual.positional_args().size(), expected.positional_args().size());
        // Then make sure each positional arg matches
        for (const auto& [a_arg, e_arg] : std::views::zip(actual.positional_args(), expected.positional_args())) {
            a_arg.visit(*this, e_arg);
        }

        // Make sure the number of named args match
        ASSERT_EQ(actual.named_args().size(), expected.named_args().size());
        // Then make sure each named arg matches
        for (const auto& [a_arg, e_arg] : std::views::zip(actual.named_args(), expected.named_args())) {
            // Confirm that the argument names are the same and are identifiers.
            EXPECT_EQ(a_arg.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_arg.name.kind(), e_arg.name.kind());
            EXPECT_EQ(a_arg.name.span(), e_arg.name.span());
            // Then compare the argument values.
            a_arg.value.visit(*this, e_arg.value);
        }
    }

    /**
     * @brief Asserts that two if expressions are equal.
     *
     * @param actual The actual if expression.
     * @param expected The expected if expression.
     */
    void operator()(const loxmocha::expr::if_t& actual, const loxmocha::expr::if_t& expected)
    {
        // Make sure the number of conditional branches match
        ASSERT_EQ(actual.conditional_branches().size(), expected.conditional_branches().size());
        // Make sure each of the branches of the if match
        for (const auto& [a_branch, e_branch] :
             std::views::zip(actual.conditional_branches(), expected.conditional_branches())) {
            a_branch.condition.visit(*this, e_branch.condition);
            a_branch.then_branch.visit(*this, e_branch.then_branch);
        }

        // If both actual and expected have else branches then compare them
        // otherwise make sure that both are null
        if (actual.else_branch() && expected.else_branch()) {
            actual.else_branch()->visit(*this, *expected.else_branch());
        } else {
            EXPECT_EQ(actual.else_branch(), nullptr);
            EXPECT_EQ(actual.else_branch(), expected.else_branch());
        }
    }

    /**
     * @brief Asserts that two while expressions are equal.
     *
     * @param actual The actual while expression.
     * @param expected The expected while expression.
     */
    void operator()(const loxmocha::expr::while_t& actual, const loxmocha::expr::while_t& expected)
    {
        actual.condition()->visit(*this, *expected.condition());
        actual.body()->visit(*this, *expected.body());
    }

    /**
     * @brief Asserts that two block expressions are equal.
     *
     * @param actual The actual block expression.
     * @param expected The expected block expression.
     */
    void operator()(const loxmocha::expr::block_t& actual, const loxmocha::expr::block_t& expected)
    {
        // Make sure the number of statements match
        ASSERT_EQ(actual.statements().size(), expected.statements().size());
        // Then make sure each statement matches
        for (const auto& [a_stmt, e_stmt] : std::views::zip(actual.statements(), expected.statements())) {
            a_stmt.visit(*this, e_stmt);
        }
    }

    /**
     * @brief Asserts that two grouping expressions are equal.
     *
     * @param actual The actual grouping expression.
     * @param expected The expected grouping expression.
     */
    void operator()(const loxmocha::expr::grouping_t& actual, const loxmocha::expr::grouping_t& expected)
    {
        actual.expression()->visit(*this, *expected.expression());
    }

    /**
     * @brief Asserts that two identifier patterns are equal.
     *
     * @param actual The actual identifier pattern.
     * @param expected The expected identifier pattern.
     */
    void operator()(const loxmocha::pattern::identifier_t& actual, const loxmocha::pattern::identifier_t& expected)
    {
        // Confirm that the names are the same and are identifiers.
        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    /**
     * @brief Asserts that two tag patterns are equal.
     *
     * @param actual The actual tag pattern.
     * @param expected The expected tag pattern.
     */
    void operator()(const loxmocha::pattern::tag_t& actual, const loxmocha::pattern::tag_t& expected)
    {
        // Compare the tag type, name and pattern.
        actual.type()->visit(*this, *expected.type());

        // Make sure the names of the tag are the same and are identifiers.
        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());

        // Compare the sub patterns of the tag.
        actual.pattern()->visit(*this, *expected.pattern());
    }

    /**
     * @brief Asserts that two expression statements are equal.
     *
     * @param actual The actual expression statement.
     * @param expected The expected expression statement.
     */
    void operator()(const loxmocha::stmt::expr_t& actual, const loxmocha::stmt::expr_t& expected)
    {
        actual.expr()->visit(*this, *expected.expr());
    }

    /**
     * @brief Asserts that two assignment statements are equal.
     *
     * @param actual The actual assignment statement.
     * @param expected The expected assignment statement.
     */
    void operator()(const loxmocha::stmt::assign_t& actual, const loxmocha::stmt::assign_t& expected)
    {
        actual.target()->visit(*this, *expected.target());
        actual.value()->visit(*this, *expected.value());
    }

    /**
     * @brief Asserts that two declaration statements are equal.
     *
     * @param actual The actual declaration statement.
     * @param expected The expected declaration statement.
     */
    void operator()(const loxmocha::stmt::decl_t& actual, const loxmocha::stmt::decl_t& expected)
    {
        actual.declaration()->visit(*this, *expected.declaration());
    }

    /**
     * @brief Asserts that two identifier types are equal.
     *
     * @param actual The actual identifier type.
     * @param expected The expected identifier type.
     */
    void operator()(const loxmocha::type::identifier_t& actual, const loxmocha::type::identifier_t& expected)
    {
        // Confirm that the names are the same and are identifiers.
        EXPECT_EQ(actual.name().kind(), loxmocha::token_t::kind_e::k_identifier);
        EXPECT_EQ(actual.name().kind(), expected.name().kind());
        EXPECT_EQ(actual.name().span(), expected.name().span());
    }

    /**
     * @brief Asserts that two array types are equal.
     *
     * @param actual The actual array type.
     * @param expected The expected array type.
     */
    void operator()(const loxmocha::type::array_t& actual, const loxmocha::type::array_t& expected)
    {
        actual.element_type()->visit(*this, *expected.element_type());
    }

    /**
     * @brief Asserts that two tuple types are equal.
     *
     * @param actual The actual tuple type.
     * @param expected The expected tuple type.
     */
    void operator()(const loxmocha::type::tuple_t& actual, const loxmocha::type::tuple_t& expected)
    {
        // Make sure they have the same number of element types.
        ASSERT_EQ(actual.element_types().size(), expected.element_types().size());
        // Then make sure each element type matches.
        for (const auto& [a_type, e_type] : std::views::zip(actual.element_types(), expected.element_types())) {
            a_type.visit(*this, e_type);
        }
    }

    /**
     * @brief Asserts that two record types are equal.
     *
     * @param actual The actual record type.
     * @param expected The expected record type.
     */
    void operator()(const loxmocha::type::record_t& actual, const loxmocha::type::record_t& expected)
    {
        // Make sure they have the same number of fields.
        ASSERT_EQ(actual.fields().size(), expected.fields().size());
        // Then make sure each field matches.
        for (const auto& [a_field, e_field] : std::views::zip(actual.fields(), expected.fields())) {
            // Confirm that the field names are the same and are identifiers.
            EXPECT_EQ(a_field.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_field.name.kind(), e_field.name.kind());
            EXPECT_EQ(a_field.name.span(), e_field.name.span());
            a_field.type.visit(*this, e_field.type);
        }
    }

    /**
     * @brief Asserts that two tagged types are equal.
     *
     * @param actual The actual tagged type.
     * @param expected The expected tagged type.
     */
    void operator()(const loxmocha::type::tagged_t& actual, const loxmocha::type::tagged_t& expected)
    {
        // Make sure they have the same number of tags.
        ASSERT_EQ(actual.tags().size(), expected.tags().size());
        // Then make sure each tag matches.
        for (const auto& [a_tag, e_tag] : std::views::zip(actual.tags(), expected.tags())) {
            // Confirm that the tag names are the same and are identifiers.
            EXPECT_EQ(a_tag.name.kind(), loxmocha::token_t::kind_e::k_identifier);
            EXPECT_EQ(a_tag.name.kind(), e_tag.name.kind());
            EXPECT_EQ(a_tag.name.span(), e_tag.name.span());
            a_tag.type.visit(*this, e_tag.type);
        }
    }

    /**
     * @brief Asserts that two reference types are equal.
     *
     * @param actual The actual reference type.
     * @param expected The expected reference type.
     */
    void operator()(const loxmocha::type::reference_t& actual, const loxmocha::type::reference_t& expected)
    {
        actual.base_type()->visit(*this, *expected.base_type());
    }

    /**
     * @brief Asserts that two function types are equal.
     *
     * @param actual The actual function type.
     * @param expected The expected function type.
     */
    void operator()(const loxmocha::type::function_t& actual, const loxmocha::type::function_t& expected)
    {
        // Make sure they have the same number of parameter types.
        ASSERT_EQ(actual.parameters().size(), expected.parameters().size());
        // Then make sure each parameter type matches.
        for (const auto& [a_type, e_type] : std::views::zip(actual.parameters(), expected.parameters())) {
            a_type.visit(*this, e_type);
        }

        // Finally make sure the return types match.
        actual.return_type()->visit(*this, *expected.return_type());
    }

    /**
     * @brief Asserts that two mutable types are equal.
     *
     * @param actual The actual mutable type.
     * @param expected The expected mutable type.
     */
    void operator()(const loxmocha::type::mutable_t& actual, const loxmocha::type::mutable_t& expected)
    {
        actual.base_type()->visit(*this, *expected.base_type());
    }
};

} // namespace loxmocha::test
