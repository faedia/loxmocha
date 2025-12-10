#pragma once

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"

#include <string>
#include <vector>

namespace loxmocha {

/**
 * @class parser_result_t
 *
 * @brief Represents the result of a parsing operation.
 * @tparam T The type of the parsed AST node.
 *
 * This class encapsulates the result of parsing, including the parsed AST node,
 * a flag indicating whether an error occurred, and any diagnostic messages.
 */
template<typename T>
class parser_result_t {
public:
    /**
     * @brief Check if the parsing was successful.
     *
     * @return true if parsing succeeded without errors, false otherwise.
     */
    explicit operator bool() const { return !has_error_; }

    /**
     * @brief Get the parsed result.
     *
     * @return const T& The parsed AST node.
     */
    [[nodiscard]] auto result() const -> const T& { return result_; }
    /**
     * @brief Get the parsed result.
     *
     * @return T& The parsed AST node.
     */
    [[nodiscard]] auto result() -> T& { return result_; }

    /**
     * @brief Get the diagnostic messages.
     * @return const std::vector<std::string>& The diagnostic messages.
     */
    [[nodiscard]] auto diagnostics() const -> const std::vector<std::string>& { return diagnostics_; }
    /**
     * @brief Get the diagnostic messages.
     * @return std::vector<std::string>& The diagnostic messages.
     */
    [[nodiscard]] auto diagnostics() -> std::vector<std::string>& { return diagnostics_; }

    /**
     * @brief Constructs a parser result with the given result, error flag, and diagnostics.
     *
     * @param result The parsed AST node.
     * @param has_error Flag indicating whether an error occurred during parsing.
     * @param diagnostics The diagnostic messages generated during parsing.
     */
    explicit parser_result_t(T&& result, bool has_error, std::vector<std::string>&& diagnostics)
        : result_(std::move(result)), has_error_(has_error), diagnostics_(std::move(diagnostics))
    {
    }

private:
    T    result_;
    bool has_error_;

    std::vector<std::string> diagnostics_;
};

auto parse_module(lexer_t& lexer) -> parser_result_t<module::module_t>;

/**
 * @brief Parse a single declaration from the given lexer.
 *
 * If a declaration is parsed successfully, the resulting AST node is returned.
 * If an error occurs during parsing, the parser result will indicate failure
 * and contain diagnostic messages.
 *
 * @param lexer The lexer to parse from.
 *
 * @return parser_result_t<decl::decl_t> The result of parsing the declaration.
 *
 */
auto parse_decl(lexer_t& lexer) -> parser_result_t<decl::decl_t>;

/**
 * @brief Parse a single statement from the given lexer.
 *
 * If a statement is parsed successfully, the resulting AST node is returned.
 * If an error occurs during parsing, the parser result will indicate failure
 * and contain diagnostic messages.
 *
 * @param lexer The lexer to parse from.
 *
 * @return parser_result_t<stmt::stmt_t> The result of parsing the statement.
 *
 */
auto parse_stmt(lexer_t& lexer) -> parser_result_t<stmt::stmt_t>;

/**
 * @brief Parse a single expression from the given lexer.
 *
 * If an expression is parsed successfully, the resulting AST node is returned.
 * If an error occurs during parsing, the parser result will indicate failure
 * and contain diagnostic messages.
 *
 * @param lexer The lexer to parse from.
 *
 * @return parser_result_t<expr::expr_t> The result of parsing the expression.
 *
 */
auto parse_expr(lexer_t& lexer) -> parser_result_t<expr::expr_t>;

/**
 * @brief Parse a single type expression from the given lexer.
 *
 * If a type expression is parsed successfully, the resulting AST node is returned.
 * If an error occurs during parsing, the parser result will indicate failure
 * and contain diagnostic messages.
 *
 * @param lexer The lexer to parse from.
 *
 * @return parser_result_t<type::type_t> The result of parsing the type expression.
 *
 */
auto parse_type(lexer_t& lexer) -> parser_result_t<type::type_t>;

/**
 * @brief Parse a single pattern from the given lexer.
 *
 * If a pattern is parsed successfully, the resulting AST node is returned.
 * If an error occurs during parsing, the parser result will indicate failure
 * and contain diagnostic messages.
 *
 * @param lexer The lexer to parse from.
 *
 * @return parser_result_t<pattern::pattern_t> The result of parsing the pattern.
 *
 */
auto parse_pattern(lexer_t& lexer) -> parser_result_t<pattern::pattern_t>;

} // namespace loxmocha
