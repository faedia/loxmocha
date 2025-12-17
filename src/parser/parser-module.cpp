#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/parser.hpp"
#include "parser-internal.hpp"

#include <vector>
#include <utility>

namespace loxmocha::internal {
auto parser_t::parse_module() -> parser_result_t<module::module_t>
{
    has_error_ = false;
    diagnostics_.clear();
    return parser_result_t<module::module_t>{parse_module_internal(), has_error_, std::move(diagnostics_)};
}

auto parser_t::parse_module_internal() -> module::module_t
{
    std::vector<decl::decl_t> declarations{};

    const auto* start_pos = lexer_.current_pos();

    while (true) {
        auto token = lexer_.peek_token();
        if (!token || token->kind() == token_t::kind_e::s_eof) {
            break;
        }

        declarations.emplace_back(parse_decl_internal());

        if (!expect_token<token_t::kind_e::p_semicolon>()) {
            diagnostics_.emplace_back("Expected ';' after declaration");
            has_error_ = true;
        }
    }

    const auto* end_pos = lexer_.current_pos();

    return module::module_t{node_base_t{start_pos, end_pos}, std::move(declarations)};
}

} // namespace loxmocha::internal
