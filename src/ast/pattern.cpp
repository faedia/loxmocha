#include "loxmocha/ast/pattern.hpp"

#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <utility>

namespace loxmocha::ast::pattern {

using namespace loxmocha::lexer;

tag_t::tag_t(safe_ptr<type::type_t>&& type, const token_t& name, safe_ptr<pattern_t>&& pattern)
    : type_(std::move(type)), name_(name), pattern_(std::move(pattern))
{
}

tag_t::~tag_t() = default;

} // namespace loxmocha::ast::pattern
