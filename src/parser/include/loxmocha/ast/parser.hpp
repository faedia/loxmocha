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

template<typename T>
class parser_result_t {
public:
    explicit operator bool() const { return !has_error_; }

    [[nodiscard]] auto result() const -> const T& { return result_; }
    [[nodiscard]] auto result() -> T& { return result_; }

    [[nodiscard]] auto diagnostics() const -> const std::vector<std::string>& { return diagnostics_; }
    [[nodiscard]] auto diagnostics() -> std::vector<std::string>& { return diagnostics_; }

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
auto parse_decl(lexer_t& lexer) -> parser_result_t<decl::decl_t>;
auto parse_stmt(lexer_t& lexer) -> parser_result_t<stmt::stmt_t>;
auto parse_expr(lexer_t& lexer) -> parser_result_t<expr::expr_t>;
auto parse_type(lexer_t& lexer) -> parser_result_t<type::type_t>;
auto parse_pattern(lexer_t& lexer) -> parser_result_t<pattern::pattern_t>;

// TODO: Something I'm not sure about is whether the parser should be
// free functions or a class with state. Having the state makes it easier to manage
// These top level functions could create a parser object and call internal methods on it?
// And then at the end of parsing, the parse result is created and the parser destroyed, for example, the parse result
// moves out the parsed AST and diagnostics into a parse result.

} // namespace loxmocha
