#pragma once

#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/source/source.hpp"

#include <cstddef>
#include <cxxabi.h>
#include <format>
#include <ostream>

namespace loxmocha::diagram {

template<typename T>
auto type_name() -> std::string
{
    // use __cxa_demangle to demangle the name given by typeid
    // We save the pointer in a unique_ptr make sure it is freed after we copy it to a string
    const std::unique_ptr<char, void (*)(void*)> demangled_ptr(
        abi::__cxa_demangle(typeid(T).name(), nullptr, nullptr, nullptr), std::free);
    std::string result = demangled_ptr ? demangled_ptr.get() : typeid(T).name();
    return result;
}

template<typename Exporter>
class diagram_exporter_t {
public:
    static void export_ast(const auto& node, const source::source_manager_t& source, std::ostream& stream)
    {
        diagram_exporter_t<Exporter> exporter{};
        Exporter::header(stream);
        node.visit(exporter, source, stream, 1);
        Exporter::footer(stream);
    }

    auto operator()(node_base_t                     span,
                    const decl::type_t&             decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     current_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, decl, decl.identifier().span(), source, current_id, stream);
        auto [child_id, next_id] = decl.type()->visit(*this, source, stream, current_id + 1);
        Exporter::edge(current_id, child_id, stream);
        return {current_id, next_id};
    }

    auto operator()(node_base_t                     span,
                    const decl::function_t&         decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     current_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, decl, decl.identifier().span(), source, current_id, stream);
        std::size_t next_id = current_id + 1;

        for (const auto& param : decl.parameters()) {
            auto [child_id, new_next_id] = param.type.visit(*this, source, stream, next_id);
            Exporter::edge(current_id, child_id, stream);
            next_id = new_next_id;
        }

        {
            auto [child_id, new_next_id] = decl.return_type()->visit(*this, source, stream, next_id);
            Exporter::edge(current_id, child_id, stream);
            next_id = new_next_id;
        }

        {
            auto [child_id, new_next_id] = decl.body()->visit(*this, source, stream, next_id);
            Exporter::edge(current_id, child_id, stream);
            next_id = new_next_id;
        }

        return {current_id, next_id};
    }

    auto operator()(node_base_t                     span,
                    const decl::variable_t&         decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     current_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, decl, decl.identifier().span(), source, current_id, stream);

        auto [type_child_id, next_id] = decl.type()->visit(*this, source, stream, current_id + 1);
        Exporter::edge(current_id, type_child_id, stream);

        auto [init_child_id, final_next_id] = decl.initialiser()->visit(*this, source, stream, next_id);
        Exporter::edge(current_id, init_child_id, stream);

        return {current_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const decl::error_t&            decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, decl, "<error>", source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const expr::literal_t&          expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, expr.value().span(), source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const expr::identifier_t&       expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, expr.name().span(), source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const expr::binary_t&           expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, expr.op().span(), source, next_id, stream);

        auto [left_child_id, next_id_after_left] = expr.left()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, left_child_id, stream);

        auto [right_child_id, final_next_id] = expr.right()->visit(*this, source, stream, next_id_after_left);
        Exporter::edge(next_id, right_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::is_t&               expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "is", source, next_id, stream);

        auto [expr_child_id, next_id_after_expr] = expr.expr()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, expr_child_id, stream);

        auto [pattern_child_id, final_next_id] = expr.pattern()->visit(*this, source, stream, next_id_after_expr);
        Exporter::edge(next_id, pattern_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::cast_t&             expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "cast", source, next_id, stream);

        auto [expr_child_id, next_id_after_expr] = expr.expr()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, expr_child_id, stream);

        auto [type_child_id, final_next_id] = expr.type()->visit(*this, source, stream, next_id_after_expr);
        Exporter::edge(next_id, type_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::unary_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, expr.op().span(), source, next_id, stream);

        auto [operand_child_id, final_next_id] = expr.operand()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, operand_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::index_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "index", source, next_id, stream);

        auto [array_child_id, next_id_after_array] = expr.base()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, array_child_id, stream);

        auto [index_child_id, final_next_id] = expr.index()->visit(*this, source, stream, next_id_after_array);
        Exporter::edge(next_id, index_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::field_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, expr.field_name().span(), source, next_id, stream);

        auto [base_child_id, final_next_id] = expr.base()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, base_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::call_t&             expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "call", source, next_id, stream);

        auto [callee_child_id, next_id_after_callee] = expr.callee()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, callee_child_id, stream);

        std::size_t final_next_id = next_id_after_callee;

        for (const auto& argument : expr.positional_args()) {
            auto [arg_child_id, new_next_id] = argument.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, arg_child_id, stream);
            final_next_id = new_next_id;
        }

        for (const auto& argument : expr.named_args()) {
            auto [arg_child_id, new_next_id] = argument.value.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, arg_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::array_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "array", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& element : expr.elements()) {
            auto [elem_child_id, new_next_id] = element.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, elem_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::record_t&           expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "record", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& field : expr.fields()) {
            auto [field_child_id, new_next_id] = field.value.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, field_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::tuple_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "tuple", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& element : expr.elements()) {
            auto [elem_child_id, new_next_id] = element.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, elem_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::grouping_t&         expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "grouping", source, next_id, stream);

        auto [expr_child_id, final_next_id] = expr.expression()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, expr_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::if_t&               expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "if", source, next_id, stream);

        std::size_t current_id = next_id + 1;

        for (const auto& branch : expr.conditional_branches()) {
            auto [cond_child_id, next_id_after_cond] = branch.condition.visit(*this, source, stream, current_id);
            Exporter::edge(next_id, cond_child_id, stream);
            current_id = next_id_after_cond;

            auto [then_child_id, next_id_after_then] = branch.then_branch.visit(*this, source, stream, current_id);
            Exporter::edge(next_id, then_child_id, stream);
            current_id = next_id_after_then;
        }

        if (expr.else_branch()) {
            auto [else_child_id, final_next_id] = expr.else_branch()->visit(*this, source, stream, current_id);
            Exporter::edge(next_id, else_child_id, stream);
            current_id = final_next_id;
        }
        return {next_id, current_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::while_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "while", source, next_id, stream);

        auto [cond_child_id, next_id_after_cond] = expr.condition()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, cond_child_id, stream);

        auto [body_child_id, final_next_id] = expr.body()->visit(*this, source, stream, next_id_after_cond);
        Exporter::edge(next_id, body_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::block_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "block", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& statement : expr.statements()) {
            auto [stmt_child_id, new_next_id] = statement.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, stmt_child_id, stream);
            final_next_id = new_next_id;
        }

        {
            auto [return_child_id, new_next_id] = expr.return_expr()->visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, return_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::error_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, expr, "<error>", source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const pattern::identifier_t&    pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, pattern, pattern.name().span(), source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const pattern::tag_t&           pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, pattern, pattern.name().span(), source, next_id, stream);

        auto [type_child_id, next_id_after_type] = pattern.type()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, type_child_id, stream);

        auto [inner_child_id, final_next_id] = pattern.pattern()->visit(*this, source, stream, next_id_after_type);
        Exporter::edge(next_id, inner_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const pattern::error_t&         pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, pattern, "<error>", source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const stmt::expr_t&             stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, stmt, "expr", source, next_id, stream);

        auto [expr_child_id, final_next_id] = stmt.expr()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, expr_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const stmt::assign_t&           stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, stmt, "assign", source, next_id, stream);

        auto [target_child_id, next_id_after_target] = stmt.target()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, target_child_id, stream);

        auto [value_child_id, final_next_id] = stmt.value()->visit(*this, source, stream, next_id_after_target);
        Exporter::edge(next_id, value_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const stmt::decl_t&             stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, stmt, "decl", source, next_id, stream);

        auto [decl_child_id, final_next_id] = stmt.declaration()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, decl_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::identifier_t&       type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, type.name().span(), source, next_id, stream);
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const type::array_t&            type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "array", source, next_id, stream);

        auto [element_child_id, final_next_id] = type.element_type()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, element_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::tuple_t&            type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "tuple", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& element_type : type.element_types()) {
            auto [elem_child_id, new_next_id] = element_type.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, elem_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::record_t&           type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "record", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& field : type.fields()) {
            auto [field_child_id, new_next_id] = field.type.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, field_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::tagged_t&           type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "choice", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& variant : type.tags()) {
            auto [variant_child_id, new_next_id] = variant.type.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, variant_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::reference_t&        type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "reference", source, next_id, stream);

        auto [inner_child_id, final_next_id] = type.base_type()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, inner_child_id, stream);

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::function_t&         type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "function", source, next_id, stream);

        std::size_t final_next_id = next_id + 1;

        for (const auto& param_type : type.parameters()) {
            auto [param_child_id, new_next_id] = param_type.visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, param_child_id, stream);
            final_next_id = new_next_id;
        }

        {
            auto [return_child_id, new_next_id] = type.return_type()->visit(*this, source, stream, final_next_id);
            Exporter::edge(next_id, return_child_id, stream);
            final_next_id = new_next_id;
        }

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::mutable_t&          type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        Exporter::node(span, type, "mutable", source, next_id, stream);

        auto [inner_child_id, final_next_id] = type.base_type()->visit(*this, source, stream, next_id + 1);
        Exporter::edge(next_id, inner_child_id, stream);

        return {next_id, final_next_id};
    }
};

class dot_exporter_helpers {
public:
    static void header(std::ostream& stream) { stream << "digraph AST {\n"; }
    static void footer(std::ostream& stream) { stream << "}\n"; }

    static void node(node_base_t                     span,
                     const auto&                     node,
                     const auto&                     item_info,
                     const source::source_manager_t& source,
                     std::size_t                     node_id,
                     std::ostream&                   stream)
    {
        auto kind = type_name<decltype(node)>();
        // cppcheck-suppress internalAstError
        auto [start, end] = source.find_source(span).view().find_span_location(span).value_or(
            std::pair<source::source_location_t, source::source_location_t>{{.line_span = "", .line = 0, .column = 0},
                                                                            {.line_span = "", .line = 0, .column = 0}});
        const std::string label =
            std::format("{}: {}\\n{}:{} - {}:{}", kind, item_info, start.line, start.column, end.line, end.column);
        stream << "    node" << node_id << " [label=\"" << label << "\"];\n";
    }

    static void edge(std::size_t from_id, std::size_t to_id, std::ostream& stream)
    {
        stream << "    node" << from_id << " -> " << "node" << to_id << ";\n";
    }
};

using dot_exporter_t = diagram_exporter_t<dot_exporter_helpers>;

class d2_exporter_helpers {
public:
    static void header([[maybe_unused]] std::ostream& stream) {}
    static void footer([[maybe_unused]] std::ostream& stream) {}

    static void node(node_base_t                     span,
                     const auto&                     node,
                     const auto&                     item_info,
                     const source::source_manager_t& source,
                     std::size_t                     node_id,
                     std::ostream&                   stream)
    {
        auto kind = type_name<decltype(node)>();
        // cppcheck-suppress internalAstError
        auto [start, end] = source.find_source(span).view().find_span_location(span).value_or(
            std::pair<source::source_location_t, source::source_location_t>{{.line_span = "", .line = 0, .column = 0},
                                                                            {.line_span = "", .line = 0, .column = 0}});
        const std::string label =
            std::format("{}: {}\\n{}:{} - {}:{}", kind, item_info, start.line, start.column, end.line, end.column);
        stream << "node" << node_id << ": " << label << "\n";
    }

    static void edge(std::size_t from_id, std::size_t to_id, std::ostream& stream)
    {
        stream << "node" << from_id << " -> " << "node" << to_id << "\n";
    }
};

using d2_exporter_t = diagram_exporter_t<d2_exporter_helpers>;

} // namespace loxmocha::diagram
