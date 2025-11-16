#pragma once

#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <variant>
#include <vector>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::type {

class type_t;

class identifier_t {
public:
    identifier_t() = delete;

    identifier_t(const identifier_t&)     = delete;
    identifier_t(identifier_t&&) noexcept = default;
    ~identifier_t()                       = default;

    auto operator=(const identifier_t&) -> identifier_t&     = delete;
    auto operator=(identifier_t&&) noexcept -> identifier_t& = default;

    explicit identifier_t(const token_t& name) : name_(name) {}

    [[nodiscard]] auto name() const -> const token_t& { return name_; }
    [[nodiscard]] auto name() -> token_t& { return name_; }

private:
    token_t name_;
};

class array_t {
public:
    array_t() = delete;

    array_t(const array_t&)     = delete;
    array_t(array_t&&) noexcept = default;
    ~array_t()                  = default;

    auto operator=(const array_t&) -> array_t&     = delete;
    auto operator=(array_t&&) noexcept -> array_t& = default;

    inline array_t(safe_ptr<type_t>&& element_type, safe_ptr<expr::expr_t>&& size_expr);

    [[nodiscard]] auto element_type() const -> const safe_ptr<type_t>& { return element_type_; }
    [[nodiscard]] auto element_type() -> safe_ptr<type_t>& { return element_type_; }

    [[nodiscard]] auto size_expr() const -> const safe_ptr<expr::expr_t>& { return size_expr_; }
    [[nodiscard]] auto size_expr() -> safe_ptr<expr::expr_t>& { return size_expr_; }

private:
    safe_ptr<type_t>       element_type_;
    safe_ptr<expr::expr_t> size_expr_;
};

class tuple_t {
public:
    tuple_t() = delete;

    tuple_t(const tuple_t&)     = delete;
    tuple_t(tuple_t&&) noexcept = default;
    ~tuple_t()                  = default;

    auto operator=(const tuple_t&) -> tuple_t&     = delete;
    auto operator=(tuple_t&&) noexcept -> tuple_t& = default;

    inline explicit tuple_t(std::vector<type_t>&& element_types);

    [[nodiscard]] auto element_types() const -> const std::vector<type_t>& { return element_types_; }
    [[nodiscard]] auto element_types() -> std::vector<type_t>& { return element_types_; }

private:
    std::vector<type_t> element_types_;
};

class record_t {
public:
    record_t() = delete;

    record_t(const record_t&)     = delete;
    record_t(record_t&&) noexcept = default;
    ~record_t()                   = default;

    auto operator=(const record_t&) -> record_t&     = delete;
    auto operator=(record_t&&) noexcept -> record_t& = default;

    inline explicit record_t(std::vector<std::pair<token_t, type_t>>&& fields);

    [[nodiscard]] auto fields() const -> const std::vector<std::pair<token_t, type_t>>& { return fields_; }
    [[nodiscard]] auto fields() -> std::vector<std::pair<token_t, type_t>>& { return fields_; }

private:
    std::vector<std::pair<token_t, type_t>> fields_;
};

class tagged_t {
public:
    tagged_t() = delete;

    tagged_t(const tagged_t&)     = delete;
    tagged_t(tagged_t&&) noexcept = default;
    ~tagged_t()                   = default;

    auto operator=(const tagged_t&) -> tagged_t&     = delete;
    auto operator=(tagged_t&&) noexcept -> tagged_t& = default;

    inline explicit tagged_t(std::vector<std::pair<token_t, type_t>>&& tags);

    [[nodiscard]] auto tags() const -> const std::vector<std::pair<token_t, type_t>>& { return tags_; }
    [[nodiscard]] auto tags() -> std::vector<std::pair<token_t, type_t>>& { return tags_; }

private:
    std::vector<std::pair<token_t, type_t>> tags_;
};

class reference_t {
public:
    reference_t() = delete;

    reference_t(const reference_t&)     = delete;
    reference_t(reference_t&&) noexcept = default;
    ~reference_t()                      = default;

    auto operator=(const reference_t&) -> reference_t&     = delete;
    auto operator=(reference_t&&) noexcept -> reference_t& = default;

    inline explicit reference_t(safe_ptr<type_t>&& base_type);

    [[nodiscard]] auto base_type() const -> const safe_ptr<type_t>& { return base_type_; }
    [[nodiscard]] auto base_type() -> safe_ptr<type_t>& { return base_type_; }

private:
    safe_ptr<type_t> base_type_;
};

class function_t {
public:
    function_t() = delete;

    function_t(const function_t&)     = delete;
    function_t(function_t&&) noexcept = default;
    ~function_t()                     = default;

    auto operator=(const function_t&) -> function_t&     = delete;
    auto operator=(function_t&&) noexcept -> function_t& = default;

    inline function_t(std::vector<std::pair<token_t, type_t>>&& parameters, safe_ptr<type_t>&& return_type);

    [[nodiscard]] auto parameters() const -> const std::vector<type_t>& { return parameters_; }
    [[nodiscard]] auto parameters() -> std::vector<type_t>& { return parameters_; }

    [[nodiscard]] auto return_type() const -> const safe_ptr<type_t>& { return return_type_; }
    [[nodiscard]] auto return_type() -> safe_ptr<type_t>& { return return_type_; }

private:
    std::vector<type_t> parameters_;
    safe_ptr<type_t>    return_type_;
};

class mutable_t {
public:
    mutable_t() = delete;

    mutable_t(const mutable_t&)     = delete;
    mutable_t(mutable_t&&) noexcept = default;
    ~mutable_t()                    = default;

    auto operator=(const mutable_t&) -> mutable_t&     = delete;
    auto operator=(mutable_t&&) noexcept -> mutable_t& = default;

    inline explicit mutable_t(safe_ptr<type_t>&& base_type);

    [[nodiscard]] auto base_type() const -> const safe_ptr<type_t>& { return base_type_; }
    [[nodiscard]] auto base_type() -> safe_ptr<type_t>& { return base_type_; }

private:
    safe_ptr<type_t> base_type_;
};

class type_t {
public:
    type_t() = delete;

    type_t(const type_t&)     = delete;
    type_t(type_t&&) noexcept = default;
    ~type_t()                 = default;

    auto operator=(const type_t&) -> type_t&     = delete;
    auto operator=(type_t&&) noexcept -> type_t& = default;
    template<typename T>
    auto operator=(T&& value) -> type_t&
    {
        type_ = std::forward<T>(value);
        return *this;
    }

    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    type_t(T&& value) noexcept : type_(std::forward<T>(value))
    {
    }

    template<typename T, typename... Args>
    // NOLINTNEXTLINE(readability-named-parameter,hicpp-named-parameter)
    explicit type_t(std::in_place_type_t<T>, Args&&... args) : type_(std::in_place_type<T>, std::forward<Args>(args)...)
    {
    }

    template<typename T>
    [[nodiscard]] auto is() const -> bool
    {
        return std::holds_alternative<T>(type_);
    }

    template<typename T>
    [[nodiscard]] auto as() const -> const T&
    {
        return std::get<T>(type_);
    }

    template<typename T>
    [[nodiscard]] auto as() -> T&
    {
        return std::get<T>(type_);
    }

    template<typename Visitor>
    [[nodiscard]] auto visit(Visitor&& visitor) -> decltype(auto)
    {
        return std::visit(std::forward<Visitor>(visitor), type_);
    }

private:
    std::variant<identifier_t, array_t, tuple_t, record_t, tagged_t, reference_t, function_t, mutable_t> type_;
};

} // namespace loxmocha::type
