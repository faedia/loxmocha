#pragma once

#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"

#include <vector>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::type {
class type_t;
}

namespace loxmocha::decl {

class decl_t;

class type_t {
public:
    type_t() = delete;

    type_t(const type_t&)     = delete;
    type_t(type_t&&) noexcept = default;
    ~type_t()                 = default;

    auto operator=(const type_t&) -> type_t&     = delete;
    auto operator=(type_t&&) noexcept -> type_t& = default;

    explicit type_t(safe_ptr<type::type_t>&& type);

    [[nodiscard]] auto identifier() const -> const token_t& { return identifier_; }
    [[nodiscard]] auto identifier() -> token_t& { return identifier_; }

    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    token_t                identifier_;
    safe_ptr<type::type_t> type_;
};

class function_t {
public:
    function_t() = delete;

    function_t(const function_t&)     = delete;
    function_t(function_t&&) noexcept = default;
    ~function_t()                     = default;

    auto operator=(const function_t&) -> function_t&     = delete;
    auto operator=(function_t&&) noexcept -> function_t& = default;

    struct parameter_t {
        token_t                name;
        safe_ptr<type::type_t> type;
    };

    explicit function_t(token_t                    identifier,
                        std::vector<parameter_t>&& parameters,
                        safe_ptr<type::type_t>&&   return_type,
                        safe_ptr<expr::expr_t>&&   body);

    [[nodiscard]] auto identifier() const -> const token_t& { return identifier_; }
    [[nodiscard]] auto identifier() -> token_t& { return identifier_; }

    [[nodiscard]] auto parameters() const -> const std::vector<parameter_t>& { return parameters_; }
    [[nodiscard]] auto parameters() -> std::vector<parameter_t>& { return parameters_; }

    [[nodiscard]] auto return_type() const -> const safe_ptr<type::type_t>& { return return_type_; }
    [[nodiscard]] auto return_type() -> safe_ptr<type::type_t>& { return return_type_; }

    [[nodiscard]] auto body() const -> const safe_ptr<expr::expr_t>& { return body_; }
    [[nodiscard]] auto body() -> safe_ptr<expr::expr_t>& { return body_; }

private:
    token_t                  identifier_;
    std::vector<parameter_t> parameters_;
    safe_ptr<type::type_t>   return_type_;
    safe_ptr<expr::expr_t>   body_;
};

class variable_t {
public:
    variable_t() = delete;

    variable_t(const variable_t&)     = delete;
    variable_t(variable_t&&) noexcept = default;
    ~variable_t()                     = default;

    auto operator=(const variable_t&) -> variable_t&     = delete;
    auto operator=(variable_t&&) noexcept -> variable_t& = default;

    explicit variable_t(token_t identifier, safe_ptr<type::type_t>&& type);

    [[nodiscard]] auto identifier() const -> const token_t& { return identifier_; }
    [[nodiscard]] auto identifier() -> token_t& { return identifier_; }

    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    token_t                identifier_;
    safe_ptr<type::type_t> type_;
};

class error_t {
public:
    error_t() = delete;

    error_t(const error_t&)     = delete;
    error_t(error_t&&) noexcept = default;
    ~error_t()                  = default;

    auto operator=(const error_t&) -> error_t&     = delete;
    auto operator=(error_t&&) noexcept -> error_t& = default;

    explicit error_t(std::string message) : message_(std::move(message)) {}

    [[nodiscard]] auto message() const -> const std::string& { return message_; }
    [[nodiscard]] auto message() -> std::string& { return message_; }

private:
    std::string message_;
};

class decl_t {
public:
    decl_t() = delete;

    decl_t(const decl_t&)     = delete;
    decl_t(decl_t&&) noexcept = default;
    ~decl_t()                 = default;

    auto operator=(const decl_t&) -> decl_t&     = delete;
    auto operator=(decl_t&&) noexcept -> decl_t& = default;
    template<typename T>
    auto operator=(T&& other) noexcept -> decl_t&
    {
        decl_ = std::forward<T>(other);
        return *this;
    }

    template<typename T>
    // cppcheck-suppress noExplicitConstructor
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload,hicpp-explicit-conversions)
    decl_t(T&& value) noexcept : decl_(std::forward<T>(value))
    {
    }

    template<typename T, typename... Args>
    // NOLINTNEXTLINE(readability-named-parameter,hicpp-named-parameter)
    explicit decl_t(std::in_place_type_t<T>, Args&&... args) : decl_(std::in_place_type<T>, std::forward<Args>(args)...)
    {
    }

    template<typename T>
    [[nodiscard]] auto is() const -> bool
    {
        return std::holds_alternative<T>(decl_);
    }

    template<typename T>
    [[nodiscard]] auto as() -> T&
    {
        return std::get<T>(decl_);
    }

    template<typename T>
    [[nodiscard]] auto as() const -> const T&
    {
        return std::get<T>(decl_);
    }

    template<typename Visitor>
    [[nodiscard]] auto visit(Visitor&& visitor) -> decltype(auto)
    {
        return std::visit(std::forward<Visitor>(visitor), decl_);
    }

private:
    std::variant<type_t, function_t, variable_t, error_t> decl_;
};

} // namespace loxmocha::decl
