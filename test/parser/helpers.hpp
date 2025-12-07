#pragma once

#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <vector>

namespace loxmocha::test::helpers {

[[nodiscard]] inline auto e(loxmocha::expr::expr_t&& expr) -> loxmocha::safe_ptr<loxmocha::expr::expr_t>
{
    return loxmocha::safe_ptr<loxmocha::expr::expr_t>::make(std::move(expr));
}

[[nodiscard]] inline auto p(loxmocha::pattern::pattern_t&& pattern) -> loxmocha::safe_ptr<loxmocha::pattern::pattern_t>
{
    return loxmocha::safe_ptr<loxmocha::pattern::pattern_t>::make(std::move(pattern));
}

[[nodiscard]] inline auto t(loxmocha::type::type_t&& type) -> loxmocha::safe_ptr<loxmocha::type::type_t>
{
    return loxmocha::safe_ptr<loxmocha::type::type_t>::make(std::move(type));
}

template<typename T, typename... Args>
auto make_vector(Args&&... args) -> std::vector<T>
{
    std::vector<T> vec;
    vec.reserve(sizeof...(Args));
    (vec.emplace_back(std::forward<Args>(args)), ...);
    return vec;
}
} // namespace loxmocha::test::helpers
