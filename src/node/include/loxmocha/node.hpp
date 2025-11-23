#pragma once

#include <variant>

/**
 * @class node_t
 * @brief A generic node class that can hold different kinds of nodes.
 *
 * @tparam Kinds The different kinds of nodes that can be held.
 */
template<typename... Kinds>
class node_t {
public:
    node_t() = delete;

    node_t(const node_t&)     = default;
    node_t(node_t&&) noexcept = default;
    ~node_t()                 = default;

    auto operator=(const node_t&) -> node_t&     = default;
    auto operator=(node_t&&) noexcept -> node_t& = default;

    /**
     * @brief Assign a new value to the node.
     *
     * @tparam T The kind of the value to assign.
     * @param value The value to assign.
     */
    template<typename T>
    auto operator=(const T& value) -> node_t&
    {
        node_ = std::forward<T>(value);
        return *this;
    }

    /**
     * @brief Assign a new value to the node.
     *
     * @tparam T The kind of the value to assign.
     * @param value The value to assign.
     */
    template<typename T>
    auto operator=(T&& value) -> node_t&
    {
        node_ = std::forward<T>(value);
        return *this;
    }

    /**
     * @brief Constructs a node from a specific kind.
     *
     * @tparam T The specific kind to construct the node from.
     * @param value The specific kind value.
     */
    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    node_t(const T& value) : node_(value)
    {
    }

    /**
     * @brief Constructs a node from a specific kind.
     *
     * @param T The specific kind to construct the node from.
     * @param value The specific kind value.
     */
    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    node_t(T&& value) : node_(std::forward<T>(value))
    {
    }

    /**
     * @brief Constructs a node in-place from the specified kind and arguments.
     *
     * @tparam T The specific kind.
     * @tparam Args The types of the arguments to construct the kind.
     * @param args The arguments to construct the kind.
     */
    template<typename T, typename... Args>
    explicit node_t([[maybe_unused]] std::in_place_type_t<T> marker, Args&&... args)
        : node_(std::in_place_type<T>, std::forward<Args>(args)...)
    {
    }

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
        return std::visit([&visitor, &args...](auto&& arg) { return visitor(arg, std::forward<Args>(args)...); },
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
        return std::visit([&visitor, &args...](auto&& arg) { return visitor(arg, std::forward<Args>(args)...); },
                          node_);
    }

private:
    std::variant<Kinds...> node_;
};