#pragma once

#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/token.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/node.hpp"

#include <utility>
#include <vector>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::type {
class type_t;
}

namespace loxmocha::decl {

class decl_t;

/**
 * @class type_t
 * @brief represents a type declaration
 */
class type_t {
public:
    type_t() = delete;

    type_t(const type_t&)     = delete;
    type_t(type_t&&) noexcept = default;
    ~type_t()                 = default;

    auto operator=(const type_t&) -> type_t&     = delete;
    auto operator=(type_t&&) noexcept -> type_t& = default;

    /**
     * @brief Constructs a type declaration from an identifier and a type.
     *
     * @param identifier The identifier of the type.
     * @param type The type associated with the identifier.
     */
    explicit type_t(token_t identifier, safe_ptr<type::type_t>&& type);

    /**
     * @brief Get the identifier of the type declaration.
     * @return const token_t& The identifier of the type declaration.
     */
    [[nodiscard]] auto identifier() const -> const token_t& { return identifier_; }
    /**
     * @brief Get the identifier of the type declaration.
     * @return token_t& The identifier of the type declaration.
     */
    [[nodiscard]] auto identifier() -> token_t& { return identifier_; }

    /**
     * @brief Get the type associated with the type declaration.
     * @return const safe_ptr<type::type_t>& The type associated with the type declaration.
     */
    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    /**
     * @brief Get the type associated with the type declaration.
     * @return safe_ptr<type::type_t>& The type associated with the type declaration.
     */
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

private:
    token_t                identifier_;
    safe_ptr<type::type_t> type_;
};

/**
 * @class function_t
 * @brief represents a function declaration
 */
class function_t {
public:
    function_t() = delete;

    function_t(const function_t&)     = delete;
    function_t(function_t&&) noexcept = default;
    ~function_t()                     = default;

    auto operator=(const function_t&) -> function_t&     = delete;
    auto operator=(function_t&&) noexcept -> function_t& = default;

    /**
     * @brief Struct representing a function parameter.
     */
    struct parameter_t {
        token_t name;
        safe_ptr<type::type_t>       type;
    };

    /**
     * @brief Constructs a function declaration from an identifier, parameters, return type, and body.
     *
     * @param identifier The identifier of the function.
     * @param parameters The parameters of the function.
     * @param return_type The return type of the function.
     * @param body The body of the function.
     */
    explicit function_t(token_t                    identifier,
                        std::vector<parameter_t>&& parameters,
                        safe_ptr<type::type_t>&&   return_type,
                        safe_ptr<expr::expr_t>&&   body);

    /**
     * @brief Get the identifier of the function declaration.
     * @return const token_t& The identifier of the function declaration.
     */
    [[nodiscard]] auto identifier() const -> const token_t& { return identifier_; }
    /**
     * @brief Get the identifier of the function declaration.
     * @return token_t& The identifier of the function declaration.
     */
    [[nodiscard]] auto identifier() -> token_t& { return identifier_; }

    /**
     * @brief Get the parameters of the function declaration.
     * @return const std::vector<parameter_t>& The parameters of the function declaration.
     */
    [[nodiscard]] auto parameters() const -> const std::vector<parameter_t>& { return parameters_; }
    /**
     * @brief Get the parameters of the function declaration.
     * @return std::vector<parameter_t>& The parameters of the function declaration.
     */
    [[nodiscard]] auto parameters() -> std::vector<parameter_t>& { return parameters_; }

    /**
     * @brief Get the return type of the function declaration.
     * @return const safe_ptr<type::type_t>& The return type of the function declaration.
     */
    [[nodiscard]] auto return_type() const -> const safe_ptr<type::type_t>& { return return_type_; }
    /**
     * @brief Get the return type of the function declaration.
     * @return safe_ptr<type::type_t>& The return type of the function declaration.
     */
    [[nodiscard]] auto return_type() -> safe_ptr<type::type_t>& { return return_type_; }

    /**
     * @brief Get the body of the function declaration.
     * @return const safe_ptr<expr::expr_t>& The body of the function declaration.
     */
    [[nodiscard]] auto body() const -> const safe_ptr<expr::expr_t>& { return body_; }
    /**
     * @brief Get the body of the function declaration.
     * @return safe_ptr<expr::expr_t>& The body of the function declaration.
     */
    [[nodiscard]] auto body() -> safe_ptr<expr::expr_t>& { return body_; }

private:
    token_t                  identifier_;
    std::vector<parameter_t> parameters_;
    safe_ptr<type::type_t>   return_type_;
    safe_ptr<expr::expr_t>   body_;
};

/**
 * @class variable_t
 * @brief represents a variable declaration
 */
class variable_t {
public:
    variable_t() = delete;

    variable_t(const variable_t&)     = delete;
    variable_t(variable_t&&) noexcept = default;
    ~variable_t()                     = default;

    auto operator=(const variable_t&) -> variable_t&     = delete;
    auto operator=(variable_t&&) noexcept -> variable_t& = default;

    enum class mut_e : std::uint8_t { var, let };

    /**
     * @brief Constructs a variable declaration from an identifier and a type.
     *
     * @param identifier The identifier of the variable.
     * @param type The type of the variable.
     * @param initialiser The initialiser expression for the variable.
     */
    explicit variable_t(mut_e                    mut,
                        token_t                  identifier,
                        safe_ptr<type::type_t>&& type,
                        safe_ptr<expr::expr_t>&& initialiser);

    /**
     * @brief Get the mutability of the variable declaration.
     * @return mut_e The mutability of the variable declaration.
     */
    [[nodiscard]] auto mutability() const -> mut_e { return mutability_; }
    /**
     * @brief Get the mutability of the variable declaration.
     * @return mut_e& The mutability of the variable declaration.
     */
    [[nodiscard]] auto mutability() -> mut_e& { return mutability_; }

    /**
     * @brief Get the identifier of the variable declaration.
     * @return const token_t& The identifier of the variable declaration.
     */
    [[nodiscard]] auto identifier() const -> const token_t& { return identifier_; }
    /**
     * @brief Get the identifier of the variable declaration.
     * @return token_t& The identifier of the variable declaration.
     */
    [[nodiscard]] auto identifier() -> token_t& { return identifier_; }

    /**
     * @brief Get the type of the variable declaration.
     * @return const safe_ptr<type::type_t>& The type of the variable declaration.
     */
    [[nodiscard]] auto type() const -> const safe_ptr<type::type_t>& { return type_; }
    /**
     * @brief Get the type of the variable declaration.
     * @return safe_ptr<type::type_t>& The type of the variable declaration.
     */
    [[nodiscard]] auto type() -> safe_ptr<type::type_t>& { return type_; }

    /**
     * @brief Get the initialiser expression of the variable declaration.
     * @return const safe_ptr<expr::expr_t>& The initialiser expression.
     */
    [[nodiscard]] auto initialiser() const -> const safe_ptr<expr::expr_t>& { return initialiser_; }
    /**
     * @brief Get the initialiser expression of the variable declaration.
     * @return safe_ptr<expr::expr_t>& The initialiser expression.
     */
    [[nodiscard]] auto initialiser() -> safe_ptr<expr::expr_t>& { return initialiser_; }

private:
    mut_e                  mutability_;
    token_t                identifier_;
    safe_ptr<type::type_t> type_;
    safe_ptr<expr::expr_t> initialiser_;
};

/**
 * @class error_t
 * @brief represents an error in a declaration
 */
class error_t {
public:
    error_t() = delete;

    error_t(const error_t&)     = default;
    error_t(error_t&&) noexcept = default;
    ~error_t()                  = default;

    auto operator=(const error_t&) -> error_t&     = default;
    auto operator=(error_t&&) noexcept -> error_t& = default;

    /**
     * @brief Constructs an error declaration with a message.
     *
     * @param message The error message.
     */
    explicit error_t(std::string message) : message_(std::move(message)) {}

    /**
     * @brief Get the error message.
     * @return const std::string& The error message.
     */
    [[nodiscard]] auto message() const -> const std::string& { return message_; }
    /**
     * @brief Get the error message.
     * @return std::string& The error message.
     */
    [[nodiscard]] auto message() -> std::string& { return message_; }

private:
    std::string message_;
};

/**
 * @class decl_t
 * @brief represents a declaration
 */
class decl_t : public node_t<type_t, function_t, variable_t, error_t> {
public:
    using node_t::node_t;
};

} // namespace loxmocha::decl
