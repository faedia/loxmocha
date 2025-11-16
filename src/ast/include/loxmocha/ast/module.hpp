#pragma once

#include "loxmocha/ast/decl.hpp"

namespace loxmocha::module {

class module_t {
public:
    module_t()                = default;
    module_t(const module_t&) = delete;
    module_t(module_t&&)      = default;
    ~module_t()               = default;

    auto operator=(const module_t&) -> module_t& = delete;
    auto operator=(module_t&&) -> module_t&      = default;

    explicit module_t(std::vector<decl::decl_t>&& declarations);

    [[nodiscard]] auto declarations() const -> const std::vector<decl::decl_t>& { return declarations_; }
    [[nodiscard]] auto declarations() -> std::vector<decl::decl_t>& { return declarations_; }

private:
    std::vector<decl::decl_t> declarations_;
};

} // namespace loxmocha::module
