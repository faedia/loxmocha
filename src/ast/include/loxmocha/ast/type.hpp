#pragma once

#include "loxmocha/ast/token.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/node.hpp"

#include <variant>
#include <vector>

namespace loxmocha::expr {
class expr_t;
}

namespace loxmocha::type {

class type_t;

/**
 * @class identifier_t
 * @brief represents an identifier type expression.
 */
class identifier_t {
public:
    identifier_t() = delete;

    identifier_t(const identifier_t&)     = default;
    identifier_t(identifier_t&&) noexcept = default;
    ~identifier_t()                       = default;

    auto operator=(const identifier_t&) -> identifier_t&     = default;
    auto operator=(identifier_t&&) noexcept -> identifier_t& = default;

    /**
     * @brief Constructs an identifier type expression with the given name.
     *
     * @param name The name of the identifier.
     */
    explicit identifier_t(const token_t& name) : name_(name) {}

    /**
     * @brief Get the name of the type.
     * @return const token_t& The name of the type.
     */
    [[nodiscard]] auto name() const -> const token_t& { return name_; }
    /**
     * @brief Get the name of the type.
     * @return token_t& The name of the type.
     */
    [[nodiscard]] auto name() -> token_t& { return name_; }

private:
    token_t name_;
};

/**
 * @class array_t
 * @brief represents an array type expression.
 */
class array_t {
public:
    array_t() = delete;

    array_t(const array_t&)     = delete;
    array_t(array_t&&) noexcept = default;
    ~array_t()                  = default;

    auto operator=(const array_t&) -> array_t&     = delete;
    auto operator=(array_t&&) noexcept -> array_t& = default;

    /**
     * @brief Constructs an array type expression with the given element type and size expression.
     *
     * @param element_type The type of the array elements.
     * @param size_expr The expression representing the size of the array.
     */
    array_t(safe_ptr<type_t>&& element_type, safe_ptr<expr::expr_t>&& size_expr);

    /**
     * @brief Get the element type of the array.
     * @return const safe_ptr<type_t>& The element type of the array.
     */
    [[nodiscard]] auto element_type() const -> const safe_ptr<type_t>& { return element_type_; }
    /**
     * @brief Get the element type of the array.
     * @return safe_ptr<type_t>& The element type of the array.
     */
    [[nodiscard]] auto element_type() -> safe_ptr<type_t>& { return element_type_; }

    /**
     * @brief Get the size expression of the array.
     * @return const safe_ptr<expr::expr_t>& The size expression of the array.
     */
    [[nodiscard]] auto size_expr() const -> const safe_ptr<expr::expr_t>& { return size_expr_; }
    /**
     * @brief Get the size expression of the array.
     * @return safe_ptr<expr::expr_t>& The size expression of the array.
     */
    [[nodiscard]] auto size_expr() -> safe_ptr<expr::expr_t>& { return size_expr_; }

private:
    safe_ptr<type_t>       element_type_;
    safe_ptr<expr::expr_t> size_expr_;
};

/**
 * @class tuple_t
 * @brief represents a tuple type expression.
 */
class tuple_t {
public:
    tuple_t() = delete;

    tuple_t(const tuple_t&)     = delete;
    tuple_t(tuple_t&&) noexcept = default;
    ~tuple_t()                  = default;

    auto operator=(const tuple_t&) -> tuple_t&     = delete;
    auto operator=(tuple_t&&) noexcept -> tuple_t& = default;

    /**
     * @brief Constructs a tuple type expression with the given element types.
     * @param element_types The types of the tuple elements.
     */
    explicit tuple_t(std::vector<type_t>&& element_types);

    /**
     * @brief Get the element types of the tuple.
     * @return const std::vector<type_t>& The element types of the tuple.
     */
    [[nodiscard]] auto element_types() const -> const std::vector<type_t>& { return element_types_; }
    /**
     * @brief Get the element types of the tuple.
     * @return std::vector<type_t>& The element types of the tuple.
     */
    [[nodiscard]] auto element_types() -> std::vector<type_t>& { return element_types_; }

private:
    std::vector<type_t> element_types_;
};

/**
 * @class record_t
 * @brief represents a record type expression.
 */
class record_t {
public:
    record_t() = delete;

    record_t(const record_t&)     = delete;
    record_t(record_t&&) noexcept = default;
    ~record_t()                   = default;

    auto operator=(const record_t&) -> record_t&     = delete;
    auto operator=(record_t&&) noexcept -> record_t& = default;

    /**
     * @brief Represents a field in a record type.
     */
    struct field_t;

    /**
     * @brief Constructs a record type expression with the given fields.
     * @param fields The fields of the record.
     */
    explicit record_t(std::vector<field_t>&& fields);

    /**
     * @brief Get the fields of the record.
     * @return const std::vector<field_t>& The fields of the record.
     */
    [[nodiscard]] auto fields() const -> const std::vector<field_t>& { return fields_; }
    /**
     * @brief Get the fields of the record.
     * @return std::vector<field_t>& The fields of the record.
     */
    [[nodiscard]] auto fields() -> std::vector<field_t>& { return fields_; }

private:
    std::vector<field_t> fields_;
};

/**
 * @class tagged_t
 * @brief represents a tagged union type expression.
 */
class tagged_t {
public:
    tagged_t() = delete;

    tagged_t(const tagged_t&)     = delete;
    tagged_t(tagged_t&&) noexcept = default;
    ~tagged_t()                   = default;

    auto operator=(const tagged_t&) -> tagged_t&     = delete;
    auto operator=(tagged_t&&) noexcept -> tagged_t& = default;

    /**
     * @brief Represents a tag in a tagged union type.
     */
    struct tag_t;

    /**
     * @brief Constructs a tagged union type expression with the given tags.
     * @param tags The tags of the tagged union.
     */
    explicit tagged_t(std::vector<tag_t>&& tags);

    /**
     * @brief Get the tags of the tagged union.
     * @return const std::vector<tag_t>& The tags of the tagged union.
     */
    [[nodiscard]] auto tags() const -> const std::vector<tag_t>& { return tags_; }
    /**
     * @brief Get the tags of the tagged union.
     * @return std::vector<tag_t>& The tags of the tagged union.
     */
    [[nodiscard]] auto tags() -> std::vector<tag_t>& { return tags_; }

private:
    std::vector<tag_t> tags_;
};

/**
 * @class reference_t
 * @brief represents a reference type expression.
 */
class reference_t {
public:
    reference_t() = delete;

    reference_t(const reference_t&)     = delete;
    reference_t(reference_t&&) noexcept = default;
    ~reference_t()                      = default;

    auto operator=(const reference_t&) -> reference_t&     = delete;
    auto operator=(reference_t&&) noexcept -> reference_t& = default;

    /**
     * @brief Constructs a reference type expression with the given base type.
     * @param base_type The base type being referenced.
     */
    explicit reference_t(safe_ptr<type_t>&& base_type);

    /**
     * @brief Get the base type being referenced.
     * @return const safe_ptr<type_t>& The base type being referenced.
     */
    [[nodiscard]] auto base_type() const -> const safe_ptr<type_t>& { return base_type_; }
    /**
     * @brief Get the base type being referenced.
     * @return safe_ptr<type_t>& The base type being referenced.
     */
    [[nodiscard]] auto base_type() -> safe_ptr<type_t>& { return base_type_; }

private:
    safe_ptr<type_t> base_type_;
};

/**
 * @class function_t
 * @brief represents a function type expression.
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
     * @brief Constructs a function type expression with the given parameter types and return type.
     *
     * @param parameters The types of the function parameters.
     * @param return_type The return type of the function.
     */
    function_t(std::vector<type_t>&& parameters, safe_ptr<type_t>&& return_type);

    /**
     * @brief Get the parameter types of the function.
     * @return const std::vector<type_t>& The parameter types of the function.
     */
    [[nodiscard]] auto parameters() const -> const std::vector<type_t>& { return parameters_; }
    /**
     * @brief Get the parameter types of the function.
     * @return std::vector<type_t>& The parameter types of the function.
     */
    [[nodiscard]] auto parameters() -> std::vector<type_t>& { return parameters_; }

    /**
     * @brief Get the return type of the function.
     * @return const safe_ptr<type_t>& The return type of the function.
     */
    [[nodiscard]] auto return_type() const -> const safe_ptr<type_t>& { return return_type_; }
    /**
     * @brief Get the return type of the function.
     * @return safe_ptr<type_t>& The return type of the function.
     */
    [[nodiscard]] auto return_type() -> safe_ptr<type_t>& { return return_type_; }

private:
    std::vector<type_t> parameters_;
    safe_ptr<type_t>    return_type_;
};

/**
 * @class mutable_t
 * @brief represents a mutable type expression.
 */
class mutable_t {
public:
    mutable_t() = delete;

    mutable_t(const mutable_t&)     = delete;
    mutable_t(mutable_t&&) noexcept = default;
    ~mutable_t()                    = default;

    auto operator=(const mutable_t&) -> mutable_t&     = delete;
    auto operator=(mutable_t&&) noexcept -> mutable_t& = default;

    /**
     * @brief Constructs a mutable type expression with the given base type.
     * @param base_type The base type that is mutable.
     */
    explicit mutable_t(safe_ptr<type_t>&& base_type);

    /**
     * @brief Get the base type that is mutable.
     * @return const safe_ptr<type_t>& The base type that is mutable.
     */
    [[nodiscard]] auto base_type() const -> const safe_ptr<type_t>& { return base_type_; }
    /**
     * @brief Get the base type that is mutable.
     * @return safe_ptr<type_t>& The base type that is mutable.
     */
    [[nodiscard]] auto base_type() -> safe_ptr<type_t>& { return base_type_; }

private:
    safe_ptr<type_t> base_type_;
};

/**
 * @class type_t
 * @brief represents a type expression.
 */
class type_t : public node_t<identifier_t, array_t, tuple_t, record_t, tagged_t, reference_t, function_t, mutable_t> {
public:
    using node_t::node_t;
};

struct record_t::field_t {
    token_t name;
    type_t  type;
};

struct tagged_t::tag_t {
    token_t name;
    type_t  type;
};

} // namespace loxmocha::type
