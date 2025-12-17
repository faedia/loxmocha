#pragma once

#include "loxmocha/ast/base.hpp"
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
    explicit module_t(const node_base_t& base, std::vector<decl::decl_t>&& declarations);

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

    /**
     * @brief Visit the node with a visitor.
     *
     * @param Visitor The type of the visitor.
     * @tparam Args The types of the additional arguments to pass to the visitor.
     * @param visitor The visitor to apply to the node.
     * @param args The additional arguments to pass to the visitor.
     * @return decltype(auto) The result of applying the visitor to the node.
     */
    template<typename Visitor, typename... Args>
    auto visit(Visitor&& visitor, Args&&... args) const
    {
        return std::forward<Visitor>(visitor)(base_, *this, std::forward<Args>(args)...);
    }

    /**
     * @brief Visit the node with a visitor.
     *
     * @param Visitor The type of the visitor.
     * @tparam Args The types of the additional arguments to pass to the visitor.
     * @param visitor The visitor to apply to the node.
     * @param args The additional arguments to pass to the visitor.
     * @return decltype(auto) The result of applying the visitor to the node.
     */
    template<typename Visitor, typename... Args>
    auto visit(Visitor&& visitor, Args&&... args)
    {
        return std::forward<Visitor>(visitor)(base_, *this, std::forward<Args>(args)...);
    }

private:
    node_base_t               base_;
    std::vector<decl::decl_t> declarations_;
};

} // namespace loxmocha::module
