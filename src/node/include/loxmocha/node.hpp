#pragma once

#include <utility>
#include <variant>

/**
 * @class node_t
 * @brief A generic node class that can hold different kinds of nodes.
 *
 * @tparam Kinds The different kinds of nodes that can be held.
 */
template<typename Base, typename... Kinds>
class node_t {
public:
    node_t() = delete;

    node_t(const node_t&)     = default;
    node_t(node_t&&) noexcept = default;
    ~node_t()                 = default;

    auto operator=(const node_t&) -> node_t&     = default;
    auto operator=(node_t&&) noexcept -> node_t& = default;

    template<typename... Args>
    explicit node_t(const Base& base, Args&&... args) : base_(base), node_(std::forward<Args>(args)...)
    {
    }

    template<typename... Args>
    explicit node_t(Base&& base, Args&&... args) : base_(std::move(base)), node_(std::forward<Args>(args)...)
    {
    }

    [[nodiscard]] auto base() const -> const Base& { return base_; }
    [[nodiscard]] auto base() -> Base& { return base_; }

    /**
     * @brief Check if the node holds a specific kind.
     * @tparam T The specific kind to check for.
     * @return true if the node holds the specific kind, false otherwise.
     */
    template<typename T>
    [[nodiscard]] auto is() const -> bool
    {
        return std::holds_alternative<T>(node_);
    }

    /**
     * @brief Get the node as a specific kind.
     * @tparam T The specific kind to get.
     * @return T& The node as the specific kind.
     */
    template<typename T>
    [[nodiscard]] auto as() -> T&
    {
        return std::get<T>(node_);
    }

    /**
     * @brief Get the node as a specific kind.
     * @tparam T The specific kind to get.
     * @return T& The node as the specific kind.
     */
    template<typename T>
    [[nodiscard]] auto as() const -> const T&
    {
        return std::get<T>(node_);
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
    auto visit(Visitor&& visitor, Args&&... args) const
    {
        return std::visit([ &base = this->base_, &visitor, &args...](
                              auto&& arg) -> auto { return visitor(base, arg, std::forward<Args>(args)...); },
                          node_);
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
        return std::visit([&base = this->base_, &visitor, &args...](
                              auto&& arg) -> auto { return visitor(base, arg, std::forward<Args>(args)...); },
                          node_);
    }

private:
    Base                   base_;
    std::variant<Kinds...> node_;
};
