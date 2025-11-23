#pragma once

#include "loxmocha/ast/decl.hpp"

namespace loxmocha::module {

/**
 * @brief Represents a single module.
 */
class module_t {
public:
    module_t()                = default;
    module_t(const module_t&) = delete;
    module_t(module_t&&)      = default;
    ~module_t()               = default;

    auto operator=(const module_t&) -> module_t& = delete;
    auto operator=(module_t&&) -> module_t&      = default;

    /**
     * @brief Constructs a module from a list of declarations.
     *
     * @param declarations The declarations to include in the module.
     */
    explicit module_t(std::vector<decl::decl_t>&& declarations);

    /**
     * @brief Get the declarations in the module.
     *
     * @return const std::vector<decl::decl_t>& The declarations in the module.
     */
    [[nodiscard]] auto declarations() const -> const std::vector<decl::decl_t>& { return declarations_; }
    /**
     * @brief Get the declarations in the module.
     *
     * @return std::vector<decl::decl_t>& The declarations in the module.
     */
    [[nodiscard]] auto declarations() -> std::vector<decl::decl_t>& { return declarations_; }

private:
    std::vector<decl::decl_t> declarations_;
};

} // namespace loxmocha::module
