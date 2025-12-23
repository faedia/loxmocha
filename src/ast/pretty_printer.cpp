#include "loxmocha/ast/pretty_printer.hpp"

#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/source/source.hpp"

#include <cstddef>
#include <format>
#include <ostream>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

namespace loxmocha::ast {

namespace {
    [[nodiscard]] auto make_indent(const std::vector<bool>& indent_stack) -> std::string
    {
        std::string indent{};

        for (const auto [index, isLast] : std::views::enumerate(indent_stack)) {
            if (static_cast<std::size_t>(index) == indent_stack.size() - 1) {
                indent += isLast ? "└──" : "├──";
            } else {
                indent += isLast ? "   " : "│  ";
            }
        }

        return indent;
    }

    [[nodiscard]] auto get_location(const source::source_manager_t& source, std::string_view span) -> std::string
    {
        auto file = source.find_source(span);
        if (file == source.end()) {
            return "<unknown file>";
        }

        if (auto loc = file.view().find_span_location(span); loc) {
            return std::format("{}:{}:{}-{}:{}",
                               file.view().filepath().filename().string(),
                               loc->first.line,
                               loc->first.column,
                               loc->second.line,
                               loc->second.column);
        }

        return std::format("{}:<unknown location>", file.view().filepath().filename().string());
    }
} // namespace

void pretty_printer_t::operator()(node_base_t                     span,
                                  const module::module_t&         module,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Module: at " << get_location(source, span) << '\n';

    for (const auto& [index, decl] : std::views::enumerate(module.declarations())) {
        indent_stack_.push_back(static_cast<std::size_t>(index) == module.declarations().size() - 1);
        decl.visit(*this, source, stream);
        indent_stack_.pop_back();
    }
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const decl::type_t&             decl,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "─Type Declaration: name: \"" << decl.identifier().span() << "\" at "
           << get_location(source, span) << '\n';
    indent_stack_.push_back(true);
    decl.type()->visit(*this, source, stream);
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const decl::function_t&         decl,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Function Declaration: name: \"" << decl.identifier().span() << "\" at "
           << get_location(source, span) << '\n';

    // Parameters
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Parameters:\n";
    if (decl.parameters().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, param] : std::views::enumerate(decl.parameters())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == decl.parameters().size() - 1);
            stream << make_indent(indent_stack_) << "┬Parameter: name: \"" << param.name.span() << "\" at "
                   << get_location(source, param.name.span()) << '\n';
            indent_stack_.push_back(true);
            param.type.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();

    // Return type
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Return Type:\n";
    indent_stack_.push_back(true);
    decl.return_type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Body
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Function Body:\n";
    indent_stack_.push_back(true);
    decl.body()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const decl::variable_t&         decl,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Variable Declaration: name: \"" << decl.identifier().span() << "\" ("
           << (decl.mutability() == decl::variable_t::mut_e::let ? "let" : "var") << ") at "
           << get_location(source, span) << '\n';

    // Type
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Type:\n";
    indent_stack_.push_back(true);
    decl.type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Initialiser
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Initialiser:\n";
    indent_stack_.push_back(true);
    decl.initialiser()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const decl::error_t&            decl,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "─Error Declaration: \"" << decl.message() << "\" at "
           << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::literal_t&          expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "─Literal Expression: value: \"" << expr.value().span() << "\" at "
           << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::identifier_t&       expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "─Identifier Expression: name: \"" << expr.name().span() << "\" at "
           << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::binary_t&           expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Binary Expression: operator: \"" << expr.op().span() << "\" at "
           << get_location(source, span) << '\n';

    // Left operand
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Left Operand:\n";
    indent_stack_.push_back(true);
    expr.left()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Right operand
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Right Operand:\n";
    indent_stack_.push_back(true);
    expr.right()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::is_t&               expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Is Expression at " << get_location(source, span) << '\n';

    // Left expression
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Expression:\n";
    indent_stack_.push_back(true);
    expr.expr()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Pattern
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Pattern:\n";
    indent_stack_.push_back(true);
    expr.pattern()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::cast_t&             expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Cast Expression at " << get_location(source, span) << '\n';

    // Left expression
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Expression:\n";
    indent_stack_.push_back(true);
    expr.expr()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Type
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Type:\n";
    indent_stack_.push_back(true);
    expr.type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::unary_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Unary Expression: operator: \"" << expr.op().span() << "\" at "
           << get_location(source, span) << '\n';

    // Operand
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Operand:\n";
    indent_stack_.push_back(true);
    expr.operand()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::index_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Index Expression at " << get_location(source, span) << '\n';

    // Base expression
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Base Expression:\n";
    indent_stack_.push_back(true);
    expr.base()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Index expression
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Index Expression:\n";
    indent_stack_.push_back(true);
    expr.index()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::field_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Field Access Expression: field name: \"" << expr.field_name().span()
           << "\" at " << get_location(source, span) << '\n';

    // Base expression
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Base Expression:\n";
    indent_stack_.push_back(true);
    expr.base()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::call_t&             expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Call Expression at " << get_location(source, span) << '\n';

    // Callee expression
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Callee Expression:\n";
    indent_stack_.push_back(true);
    expr.callee()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Positional arguments
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Positional Arguments:\n";
    if (expr.positional_args().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, arg] : std::views::enumerate(expr.positional_args())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == expr.positional_args().size() - 1);
            stream << make_indent(indent_stack_) << "┬Argument " << index << ":\n";
            indent_stack_.push_back(true);
            arg.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();

    // Named arguments
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Named Arguments:\n";
    if (expr.named_args().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, arg] : std::views::enumerate(expr.named_args())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == expr.named_args().size() - 1);
            stream << make_indent(indent_stack_) << "┬Argument \"" << arg.name.span() << "\":\n";
            indent_stack_.push_back(true);
            arg.value.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::array_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Array Expression at " << get_location(source, span) << '\n';

    // Elements
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Elements:\n";
    if (expr.elements().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, element] : std::views::enumerate(expr.elements())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == expr.elements().size() - 1);
            stream << make_indent(indent_stack_) << "┬Element " << index << ":\n";
            indent_stack_.push_back(true);
            element.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::record_t&           expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Record Expression at " << get_location(source, span) << '\n';

    // Fields
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Fields:\n";
    if (expr.fields().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, field] : std::views::enumerate(expr.fields())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == expr.fields().size() - 1);
            stream << make_indent(indent_stack_) << "┬Field \"" << field.name.span() << "\":\n";
            indent_stack_.push_back(true);
            field.value.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::tuple_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Tuple Expression at " << get_location(source, span) << '\n';

    // Elements
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Elements:\n";
    if (expr.elements().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, element] : std::views::enumerate(expr.elements())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == expr.elements().size() - 1);
            stream << make_indent(indent_stack_) << "┬Element " << index << ":\n";
            indent_stack_.push_back(true);
            element.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::grouping_t&         expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Grouping Expression at " << get_location(source, span) << '\n';

    // Inner expression
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Inner Expression:\n";
    indent_stack_.push_back(true);
    expr.expression()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::if_t&               expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬If Expression at " << get_location(source, span) << '\n';

    // Conditional branches
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Conditional Branches:\n";
    for (const auto& [index, branch] : std::views::enumerate(expr.conditional_branches())) {
        indent_stack_.push_back(static_cast<std::size_t>(index) == expr.conditional_branches().size() - 1);
        stream << make_indent(indent_stack_) << "┬Branch " << index << ":\n";

        // Condition
        indent_stack_.push_back(false);
        stream << make_indent(indent_stack_) << "┬Condition:\n";
        indent_stack_.push_back(true);
        branch.condition.visit(*this, source, stream);
        indent_stack_.pop_back();
        indent_stack_.pop_back();

        // Then branch
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "┬Then Branch:\n";
        indent_stack_.push_back(true);
        branch.then_branch.visit(*this, source, stream);
        indent_stack_.pop_back();
        indent_stack_.pop_back();

        indent_stack_.pop_back();
    }
    indent_stack_.pop_back();

    // Else branch
    if (expr.else_branch()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "┬Else Branch:\n";
        indent_stack_.push_back(true);
        expr.else_branch()->visit(*this, source, stream);
        indent_stack_.pop_back();
        indent_stack_.pop_back();
    }
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::while_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬While Expression at " << get_location(source, span) << '\n';

    // Condition
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Condition:\n";
    indent_stack_.push_back(true);
    expr.condition()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Body
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Body:\n";
    indent_stack_.push_back(true);
    expr.body()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const expr::block_t&            expr,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Block Expression at " << get_location(source, span) << '\n';

    // Statements
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Statements:\n";
    if (expr.statements().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, statement] : std::views::enumerate(expr.statements())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == expr.statements().size() - 1);
            stream << make_indent(indent_stack_) << "┬Statement " << index << ":\n";
            indent_stack_.push_back(true);
            statement.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();

    // Return expression
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Return Expression:\n";
    indent_stack_.push_back(true);
    expr.return_expr()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                           span,
                                  [[maybe_unused]] const expr::error_t& expr,
                                  const source::source_manager_t&       source,
                                  std::ostream&                         stream)
{
    stream << make_indent(indent_stack_) << "─Error Expression at " << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const pattern::identifier_t&    pattern,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "─Identifier Pattern: name: \"" << pattern.name().span() << "\" at "
           << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const pattern::tag_t&           pattern,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Tag Pattern: tag name: \"" << pattern.name().span() << "\" at "
           << get_location(source, span) << '\n';

    // Type
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Type:\n";
    indent_stack_.push_back(true);
    pattern.type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Inner pattern
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Inner Pattern:\n";
    indent_stack_.push_back(true);
    pattern.pattern()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                              span,
                                  [[maybe_unused]] const pattern::error_t& pattern,
                                  const source::source_manager_t&          source,
                                  std::ostream&                            stream)
{
    stream << make_indent(indent_stack_) << "─Error Pattern at " << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const stmt::expr_t&             stmt,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Expression Statement at " << get_location(source, span) << '\n';

    // Expression
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Expression:\n";
    indent_stack_.push_back(true);
    stmt.expr()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const stmt::assign_t&           stmt,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Assignment Statement at " << get_location(source, span) << '\n';

    // Target
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Target:\n";
    indent_stack_.push_back(true);
    stmt.target()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();

    // Value
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Value:\n";
    indent_stack_.push_back(true);
    stmt.value()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const stmt::decl_t&             stmt,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Declaration Statement at " << get_location(source, span) << '\n';

    // Declaration
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Declaration:\n";
    indent_stack_.push_back(true);
    stmt.declaration()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::identifier_t&       type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "─Identifier Type: name: \"" << type.name().span() << "\" at "
           << get_location(source, span) << '\n';
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::array_t&            type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Array Type at " << get_location(source, span) << '\n';

    // Element type
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Element Type:\n";
    indent_stack_.push_back(true);
    type.element_type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::tuple_t&            type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Tuple Type at " << get_location(source, span) << '\n';

    // Element types
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Element Types:\n";
    if (type.element_types().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, element_type] : std::views::enumerate(type.element_types())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == type.element_types().size() - 1);
            stream << make_indent(indent_stack_) << "┬Element Type " << index << ":\n";
            indent_stack_.push_back(true);
            element_type.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::record_t&           type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Record Type at " << get_location(source, span) << '\n';

    // Fields
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Fields:\n";
    if (type.fields().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, field] : std::views::enumerate(type.fields())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == type.fields().size() - 1);
            stream << make_indent(indent_stack_) << "┬Field \"" << field.name.span() << "\":\n";
            indent_stack_.push_back(true);
            field.type.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::tagged_t&           type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Tagged Type at " << get_location(source, span) << '\n';

    // Choices
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Choices:\n";
    for (const auto& [index, tag] : std::views::enumerate(type.tags())) {
        indent_stack_.push_back(static_cast<std::size_t>(index) == type.tags().size() - 1);
        stream << make_indent(indent_stack_) << "┬Choice \"" << tag.name.span() << "\":\n";
        indent_stack_.push_back(true);
        tag.type.visit(*this, source, stream);
        indent_stack_.pop_back();
        indent_stack_.pop_back();
    }
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::reference_t&        type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Reference Type at " << get_location(source, span) << '\n';

    // Referenced type
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Referenced Type:\n";
    indent_stack_.push_back(true);
    type.base_type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::function_t&         type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Function Type at " << get_location(source, span) << '\n';

    // Parameter types
    indent_stack_.push_back(false);
    stream << make_indent(indent_stack_) << "┬Parameter Types:\n";
    if (type.parameters().empty()) {
        indent_stack_.push_back(true);
        stream << make_indent(indent_stack_) << "─<none>\n";
        indent_stack_.pop_back();
    } else {
        for (const auto& [index, param_type] : std::views::enumerate(type.parameters())) {
            indent_stack_.push_back(static_cast<std::size_t>(index) == type.parameters().size() - 1);
            stream << make_indent(indent_stack_) << "┬Parameter Type " << index << ":\n";
            indent_stack_.push_back(true);
            param_type.visit(*this, source, stream);
            indent_stack_.pop_back();
            indent_stack_.pop_back();
        }
    }
    indent_stack_.pop_back();

    // Return type
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Return Type:\n";
    indent_stack_.push_back(true);
    type.return_type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

void pretty_printer_t::operator()(node_base_t                     span,
                                  const type::mutable_t&          type,
                                  const source::source_manager_t& source,
                                  std::ostream&                   stream)
{
    stream << make_indent(indent_stack_) << "┬Mutable Type at " << get_location(source, span) << '\n';

    // Inner type
    indent_stack_.push_back(true);
    stream << make_indent(indent_stack_) << "┬Inner Type:\n";
    indent_stack_.push_back(true);
    type.base_type()->visit(*this, source, stream);
    indent_stack_.pop_back();
    indent_stack_.pop_back();
}

} // namespace loxmocha::ast
