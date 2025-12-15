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
#include <ostream>

namespace loxmocha::dot {

class dot_exporter_t {
    static auto demangle(const char* name) -> std::string
    {
        // use __cxa_demangle to demangle the name given by typeid
        // We save the pointer in a unique_ptr make sure it is freed after we copy it to a string
        const std::unique_ptr<char, void (*)(void*)> demangled_ptr(abi::__cxa_demangle(name, nullptr, nullptr, nullptr),
                                                                   std::free);
        std::string                                  result = demangled_ptr ? demangled_ptr.get() : name;
        return result;
    }

    static auto
    node_label(node_base_t span, const auto& node, std::string_view name, const source::source_manager_t& source)
        -> std::string
    {
        auto kind         = demangle(typeid(node).name());
        auto [start, end] = source.find_source(span).view().find_span_location(span).value_or(
            std::pair<source::source_location_t, source::source_location_t>{{.line_span = "", .line = 0, .column = 0},
                                                                            {.line_span = "", .line = 0, .column = 0}});
        return std::format(
            R"([label="{}: {}\n at {}:{} - {}:{}"])", kind, name, start.line, start.column, end.line, end.column);
    }

public:
    static void export_ast(const decl::decl_t& root, const source::source_manager_t& source, std::ostream& stream)
    {
        dot_exporter_t exporter{};
        stream << "digraph AST {\n";
        root.visit(exporter, source, stream, 1);
        stream << "}\n";
    }

    auto operator()(node_base_t                     span,
                    const decl::type_t&             decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     current_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << current_id << ' ' << node_label(span, decl, decl.identifier().span(), source) << ";\n";

        auto [child_id, next_id] = decl.type()->visit(*this, source, stream, current_id + 1);

        stream << "    node" << current_id << " -> " << "node" << child_id << ";\n";
        return {current_id, next_id};
    }

    auto operator()(node_base_t                     span,
                    const decl::function_t&         decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     current_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << current_id << ' ' << node_label(span, decl, decl.identifier().span(), source) << ";\n";

        std::size_t next_id = current_id + 1;

        for (const auto& param : decl.parameters()) {
            auto [child_id, new_next_id] = param.type.visit(*this, source, stream, next_id);
            stream << "    node" << current_id << " -> " << "node" << child_id << ";\n";
            next_id = new_next_id;
        }

        {
            auto [child_id, new_next_id] = decl.return_type()->visit(*this, source, stream, next_id);
            stream << "    node" << current_id << " -> " << "node" << child_id << ";\n";
            next_id = new_next_id;
        }

        {
            auto [child_id, new_next_id] = decl.body()->visit(*this, source, stream, next_id);
            stream << "    node" << current_id << " -> " << "node" << child_id << ";\n";
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
        stream << "    node" << current_id << ' ' << node_label(span, decl, decl.identifier().span(), source) << ";\n";

        auto [type_child_id, next_id] = decl.type()->visit(*this, source, stream, current_id + 1);
        stream << "    node" << current_id << " -> " << "node" << type_child_id << ";\n";

        auto [init_child_id, final_next_id] = decl.initialiser()->visit(*this, source, stream, next_id);
        stream << "    node" << current_id << " -> " << "node" << init_child_id << ";\n";

        return {current_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const decl::error_t&            decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, decl, "<error>", source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const expr::literal_t&          expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, expr.value().span(), source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const expr::identifier_t&       expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, expr.name().span(), source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const expr::binary_t&           expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, expr.op().span(), source) << ";\n";

        auto [left_child_id, next_id_after_left] = expr.left()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << left_child_id << ";\n";

        auto [right_child_id, final_next_id] = expr.right()->visit(*this, source, stream, next_id_after_left);
        stream << "    node" << next_id << " -> " << "node" << right_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::is_t&               expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, "is", source) << ";\n";

        auto [expr_child_id, next_id_after_expr] = expr.expr()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << expr_child_id << ";\n";

        return {next_id, next_id_after_expr};
    }

    auto operator()(node_base_t                     span,
                    const expr::cast_t&             expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, "cast", source) << ";\n";

        auto [expr_child_id, next_id_after_expr] = expr.expr()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << expr_child_id << ";\n";

        auto [type_child_id, final_next_id] = expr.type()->visit(*this, source, stream, next_id_after_expr);
        stream << "    node" << next_id << " -> " << "node" << type_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::unary_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, expr.op().span(), source) << ";\n";

        auto [operand_child_id, final_next_id] = expr.operand()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << operand_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::index_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, "index", source) << ";\n";

        auto [array_child_id, next_id_after_array] = expr.base()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << array_child_id << ";\n";

        auto [index_child_id, final_next_id] = expr.index()->visit(*this, source, stream, next_id_after_array);
        stream << "    node" << next_id << " -> " << "node" << index_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::field_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, expr.field_name().span(), source) << ";\n";

        auto [base_child_id, final_next_id] = expr.base()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << base_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::call_t&             expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, "call", source) << ";\n";

        auto [callee_child_id, next_id_after_callee] = expr.callee()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << callee_child_id << ";\n";

        std::size_t final_next_id = next_id_after_callee;

        for (const auto& argument : expr.positional_args()) {
            auto [arg_child_id, new_next_id] = argument.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << arg_child_id << ";\n";
            final_next_id = new_next_id;
        }

        for (const auto& argument : expr.named_args()) {
            auto [arg_child_id, new_next_id] = argument.value.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << arg_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, expr, "array", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& element : expr.elements()) {
            auto [elem_child_id, new_next_id] = element.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << elem_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, expr, "record", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& field : expr.fields()) {
            auto [field_child_id, new_next_id] = field.value.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << field_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, expr, "tuple", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& element : expr.elements()) {
            auto [elem_child_id, new_next_id] = element.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << elem_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, expr, "grouping", source) << ";\n";

        auto [expr_child_id, final_next_id] = expr.expression()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << expr_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::if_t&               expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, "if", source) << ";\n";

        std::size_t current_id = next_id + 1;

        for (const auto& branch : expr.conditional_branches()) {
            auto [cond_child_id, next_id_after_cond] = branch.condition.visit(*this, source, stream, current_id);
            stream << "    node" << next_id << " -> " << "node" << cond_child_id << ";\n";
            current_id = next_id_after_cond;

            auto [then_child_id, next_id_after_then] = branch.then_branch.visit(*this, source, stream, current_id);
            stream << "    node" << next_id << " -> " << "node" << then_child_id << ";\n";
            current_id = next_id_after_then;
        }

        if (expr.else_branch()) {
            auto [else_child_id, final_next_id] = expr.else_branch()->visit(*this, source, stream, current_id);
            stream << "    node" << next_id << " -> " << "node" << else_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, expr, "while", source) << ";\n";

        auto [cond_child_id, next_id_after_cond] = expr.condition()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << cond_child_id << ";\n";

        auto [body_child_id, final_next_id] = expr.body()->visit(*this, source, stream, next_id_after_cond);
        stream << "    node" << next_id << " -> " << "node" << body_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const expr::block_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, expr, "block", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& statement : expr.statements()) {
            auto [stmt_child_id, new_next_id] = statement.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << stmt_child_id << ";\n";
            final_next_id = new_next_id;
        }

        {
            auto [return_child_id, new_next_id] = expr.return_expr()->visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << return_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, expr, "<error>", source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const pattern::identifier_t&    pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, pattern, pattern.name().span(), source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const pattern::tag_t&           pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, pattern, pattern.name().span(), source) << ";\n";

        auto [type_child_id, next_id_after_type] = pattern.type()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << type_child_id << ";\n";

        auto [inner_child_id, final_next_id] = pattern.pattern()->visit(*this, source, stream, next_id_after_type);
        stream << "    node" << next_id << " -> " << "node" << inner_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const pattern::error_t&         pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, pattern, "<error>", source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const stmt::expr_t&             stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, stmt, "expr", source) << ";\n";

        auto [expr_child_id, final_next_id] = stmt.expr()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << expr_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const stmt::assign_t&           stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, stmt, "assign", source) << ";\n";

        auto [target_child_id, next_id_after_target] = stmt.target()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << target_child_id << ";\n";

        auto [value_child_id, final_next_id] = stmt.value()->visit(*this, source, stream, next_id_after_target);
        stream << "    node" << next_id << " -> " << "node" << value_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const stmt::decl_t&             stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, stmt, "decl", source) << ";\n";

        auto [decl_child_id, final_next_id] = stmt.declaration()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << decl_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::identifier_t&       type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, type, type.name().span(), source) << ";\n";
        return {next_id, next_id + 1};
    }

    auto operator()(node_base_t                     span,
                    const type::array_t&            type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, type, "array", source) << ";\n";

        auto [element_child_id, final_next_id] = type.element_type()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << element_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::tuple_t&            type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, type, "tuple", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& element_type : type.element_types()) {
            auto [elem_child_id, new_next_id] = element_type.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << elem_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, type, "record", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& field : type.fields()) {
            auto [field_child_id, new_next_id] = field.type.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << field_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, type, "choice", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& variant : type.tags()) {
            auto [variant_child_id, new_next_id] = variant.type.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << variant_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, type, "reference", source) << ";\n";

        auto [inner_child_id, final_next_id] = type.base_type()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << inner_child_id << ";\n";

        return {next_id, final_next_id};
    }

    auto operator()(node_base_t                     span,
                    const type::function_t&         type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream,
                    std::size_t                     next_id) -> std::pair<std::size_t, std::size_t>
    {
        stream << "    node" << next_id << ' ' << node_label(span, type, "function", source) << ";\n";

        std::size_t final_next_id = next_id + 1;

        for (const auto& param_type : type.parameters()) {
            auto [param_child_id, new_next_id] = param_type.visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << param_child_id << ";\n";
            final_next_id = new_next_id;
        }

        {
            auto [return_child_id, new_next_id] = type.return_type()->visit(*this, source, stream, final_next_id);
            stream << "    node" << next_id << " -> " << "node" << return_child_id << ";\n";
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
        stream << "    node" << next_id << ' ' << node_label(span, type, "mutable", source) << ";\n";

        auto [inner_child_id, final_next_id] = type.base_type()->visit(*this, source, stream, next_id + 1);
        stream << "    node" << next_id << " -> " << "node" << inner_child_id << ";\n";

        return {next_id, final_next_id};
    }
};

} // namespace loxmocha::dot
