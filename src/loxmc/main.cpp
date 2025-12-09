#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"

#include <chrono>
#include <cstddef>
#include <fstream>
#include <iterator>
#include <print>
#include <string>
#include <vector>

namespace visitor {
using namespace loxmocha;

class print_visitor {
public:
    [[nodiscard]] static auto create_indent(const std::vector<bool>& lastChildPath) -> std::string
    {
        if (lastChildPath.empty()) {
            return "";
        }

        std::string indent;
        for (size_t i = 0; i < lastChildPath.size() - 1; ++i) {
            indent += lastChildPath[i] ? "   " : "│  ";
        }
        indent += lastChildPath.back() ? "└──" : "├──";
        return indent;
    }

    void operator()(const decl::type_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Type Decl: \"{}\"\n", create_indent(lastChildPath), type.identifier().span());
        lastChildPath.push_back(true);
        type.type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const decl::function_t& func, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Function Decl: \"{}\"\n", create_indent(lastChildPath), func.identifier().span());
        lastChildPath.push_back(false);
        std::print("{}┬Parameters:\n", create_indent(lastChildPath));
        for (size_t i = 0; i < func.parameters().size(); ++i) {
            const auto& param = func.parameters()[i];
            lastChildPath.push_back(i == func.parameters().size() - 1);
            std::print("{}┬Parameter: \"{}\"\n", create_indent(lastChildPath), param.name.span());
            lastChildPath.push_back(true);
            param.type->visit(*this, lastChildPath);
            lastChildPath.pop_back();
            lastChildPath.pop_back();
        }
        lastChildPath.pop_back();

        lastChildPath.push_back(false);
        func.return_type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        func.body()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const decl::variable_t& var, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Variable Decl: \"{}\" ({})\n",
                   create_indent(lastChildPath),
                   var.identifier().span(),
                   var.mutability() == decl::variable_t::mut_e::var ? "var" : "let");

        lastChildPath.push_back(false);
        var.type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        var.initialiser()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::literal_t& literal, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Literal Expr: \"{}\"\n", create_indent(lastChildPath), literal.value().span());
    }

    void operator()(const expr::identifier_t& identifier, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Identifier Expr: \"{}\"\n", create_indent(lastChildPath), identifier.name().span());
    }

    void operator()(const expr::binary_t& binary, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Binary Expr: \"{}\"\n", create_indent(lastChildPath), binary.op().span());

        lastChildPath.push_back(false);
        binary.left()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        binary.right()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::unary_t& unary, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Unary Expr: \"{}\"\n", create_indent(lastChildPath), unary.op().span());

        lastChildPath.push_back(true);
        unary.operand()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::is_t& is_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Is Expr: \"is\"\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        is_expr.expr()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        is_expr.expr()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::cast_t& cast_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Cast Expr: \"as\"\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        cast_expr.expr()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        cast_expr.type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::array_t& array_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Array Expr:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < array_expr.elements().size(); ++i) {
            lastChildPath.push_back(i == array_expr.elements().size() - 1);
            array_expr.elements()[i].visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const expr::tuple_t& tuple_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Tuple Expr:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < tuple_expr.elements().size(); ++i) {
            lastChildPath.push_back(i == tuple_expr.elements().size() - 1);
            tuple_expr.elements()[i].visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const expr::record_t& record_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Record Expr:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < record_expr.fields().size(); ++i) {
            const auto& [field_name, field_expr] = record_expr.fields()[i];
            lastChildPath.push_back(false);
            std::print("{}├─Field Name: \"{}\"\n", create_indent(lastChildPath), field_name.span());
            lastChildPath.pop_back();

            lastChildPath.push_back(i == record_expr.fields().size() - 1);
            field_expr.visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const expr::index_t& index_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Index Expr:\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        index_expr.base()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        index_expr.index()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::field_t& field_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Field Expr: \"{}\"\n", create_indent(lastChildPath), field_expr.field_name().span());

        lastChildPath.push_back(true);
        field_expr.base()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const expr::call_t& call_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Call Expr:\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        call_expr.callee()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        for (size_t i = 0; i < call_expr.positional_args().size(); ++i) {
            lastChildPath.push_back(i == call_expr.positional_args().size() - 1 && call_expr.named_args().empty());
            call_expr.positional_args()[i].visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }

        for (size_t i = 0; i < call_expr.named_args().size(); ++i) {
            const auto& [arg_name, arg_expr] = call_expr.named_args()[i];
            lastChildPath.push_back(false);
            std::print("{}├─Named Arg: \"{}\"\n", create_indent(lastChildPath), arg_name.span());
            lastChildPath.pop_back();

            lastChildPath.push_back(i == call_expr.named_args().size() - 1);
            arg_expr.visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const expr::if_t& if_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬If Expr:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < if_expr.conditional_branches().size(); ++i) {
            const auto& branch = if_expr.conditional_branches()[i];
            lastChildPath.push_back(false);
            std::print("{}├─Condition:\n", create_indent(lastChildPath));
            lastChildPath.push_back(true);
            branch.condition->visit(*this, lastChildPath);
            lastChildPath.pop_back();
            lastChildPath.pop_back();

            lastChildPath.push_back(i == if_expr.conditional_branches().size() - 1 && !if_expr.else_branch());
            std::print("{}└─Then:\n", create_indent(lastChildPath));
            lastChildPath.push_back(true);
            branch.then_branch->visit(*this, lastChildPath);
            lastChildPath.pop_back();
            lastChildPath.pop_back();
        }

        if (if_expr.else_branch()) {
            lastChildPath.push_back(true);
            std::print("{}└─Else:\n", create_indent(lastChildPath));
            lastChildPath.push_back(true);
            if_expr.else_branch()->visit(*this, lastChildPath);
            lastChildPath.pop_back();
            lastChildPath.pop_back();
        }
    }

    void operator()(const expr::while_t& while_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬While Expr:\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        std::print("{}┬Condition:\n", create_indent(lastChildPath));
        lastChildPath.push_back(true);
        while_expr.condition()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        std::print("{}┬Body:\n", create_indent(lastChildPath));
        lastChildPath.push_back(true);
        while_expr.body()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
        lastChildPath.pop_back();
    }

    void operator()(const expr::block_t& block_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Block Expr:\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        for (const auto& stmt : block_expr.statements()) {
            stmt.visit(*this, lastChildPath);
        }
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        std::print("{}┬Return Expr:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        block_expr.return_expr()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
        lastChildPath.pop_back();
    }

    void operator()(const expr::grouping_t& grouping_expr, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Grouping Expr:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        grouping_expr.expression()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const pattern::identifier_t& pattern, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Identifier Pattern: \"{}\"\n", create_indent(lastChildPath), pattern.name().span());
    }

    void operator()(const pattern::tag_t& pattern, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Tag Pattern: \"{}\"\n", create_indent(lastChildPath), pattern.name().span());

        lastChildPath.push_back(false);
        pattern.type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        pattern.pattern()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const stmt::expr_t& stmt, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Expr Statement:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        stmt.expr()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const stmt::assign_t& stmt, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Assign Statement:\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        stmt.target()->visit(*this, lastChildPath);
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        stmt.value()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const stmt::decl_t& stmt, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Decl Statement:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        stmt.declaration()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const type::identifier_t& type, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Identifier Type: \"{}\"\n", create_indent(lastChildPath), type.name().span());
    }

    void operator()(const type::array_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Array Type:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        type.element_type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const type::tuple_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Tuple Type:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < type.element_types().size(); ++i) {
            lastChildPath.push_back(i == type.element_types().size() - 1);
            type.element_types()[i].visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const type::record_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Record Type:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < type.fields().size(); ++i) {
            const auto& [field_name, field_type] = type.fields()[i];
            lastChildPath.push_back(false);
            std::print("{}├─Field Name: \"{}\"\n", create_indent(lastChildPath), field_name.span());
            lastChildPath.pop_back();

            lastChildPath.push_back(i == type.fields().size() - 1);
            field_type.visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const type::tagged_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Tagged Type:\n", create_indent(lastChildPath));

        for (size_t i = 0; i < type.tags().size(); ++i) {
            const auto& tag = type.tags()[i];
            lastChildPath.push_back(false);
            std::print("{}├─Tag Name: \"{}\"\n", create_indent(lastChildPath), tag.name.span());
            lastChildPath.pop_back();

            lastChildPath.push_back(i == type.tags().size() - 1);
            tag.type.visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
    }

    void operator()(const type::reference_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Reference Type:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        type.base_type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const type::function_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Function Type:\n", create_indent(lastChildPath));

        lastChildPath.push_back(false);
        std::print("{}┬Parameters:\n", create_indent(lastChildPath));
        for (size_t i = 0; i < type.parameters().size(); ++i) {
            const auto& param_type = type.parameters()[i];
            lastChildPath.push_back(i == type.parameters().size() - 1);
            param_type.visit(*this, lastChildPath);
            lastChildPath.pop_back();
        }
        lastChildPath.pop_back();

        lastChildPath.push_back(true);
        std::print("{}└─Return Type:\n", create_indent(lastChildPath));
        lastChildPath.push_back(true);
        type.return_type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
        lastChildPath.pop_back();
    }

    void operator()(const type::mutable_t& type, std::vector<bool> lastChildPath = {}) const
    {
        std::print("{}┬Mutable Type:\n", create_indent(lastChildPath));

        lastChildPath.push_back(true);
        type.base_type()->visit(*this, lastChildPath);
        lastChildPath.pop_back();
    }

    void operator()(const decl::error_t& error_decl, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Error Decl: \"{}\"\n", create_indent(lastChildPath), error_decl.message());
    }

    void operator()([[maybe_unused]] const expr::error_t& error_expr, const std::vector<bool>& lastChildPath = {}) const
    {
        std::print("{}─Error Expr:\n", create_indent(lastChildPath));
    }

    void operator()([[maybe_unused]] const pattern::error_t& error_pattern,
                    const std::vector<bool>&                 lastChildPath = {}) const
    {
        std::print("{}─Error Pattern:\n", create_indent(lastChildPath));
    }
};

} // namespace visitor

auto main(int argc, const char* argv[]) -> int
{
    if (argc != 2) {
        std::print("Usage: {} <source-file>\n", *std::next(argv, 0));
        return 1;
    }

    const char* source_file = *std::next(argv, 1);

    auto file = std::ifstream{source_file};
    if (!file.is_open()) {
        std::print("Error: Could not open file {}\n", source_file);
        return 1;
    }

    std::string source_code((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

    std::print("Source Code:\n{}\n", source_code);

    auto lexer = loxmocha::lexer_t{source_code};

    auto start = std::chrono::high_resolution_clock::now();
    auto parse_result = loxmocha::parse_decl(lexer);
    auto end = std::chrono::high_resolution_clock::now();
    std::print("Parsing took {} microseconds.\n",
               std::chrono::duration_cast<std::chrono::microseconds>(end - start).count());

    if (!parse_result) {
        std::print("Parsing failed with errors:\n");
        for (const auto& diag : parse_result.diagnostics()) {
            std::print("- {}\n", diag);
        }
        return 1;
    }

    std::print("Parsing succeeded.\n");

    parse_result.result().visit(visitor::print_visitor{});

    return 0;
}
