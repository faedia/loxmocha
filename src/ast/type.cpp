#include "loxmocha/ast/type.hpp"

namespace loxmocha::type {

array_t::array_t(safe_ptr<type_t>&& element_type, safe_ptr<expr::expr_t>&& size_expr)
    : element_type_(std::move(element_type)), size_expr_(std::move(size_expr))
{
}

tuple_t::tuple_t(std::vector<type_t>&& element_types) : element_types_(std::move(element_types)) {}

record_t::record_t(std::vector<field_t>&& fields) : fields_(std::move(fields)) {}

tagged_t::tagged_t(std::vector<tag_t>&& tags) : tags_(std::move(tags)) {}

reference_t::reference_t(safe_ptr<type_t>&& base_type) : base_type_(std::move(base_type)) {}

function_t::function_t(std::vector<type_t>&& parameters, safe_ptr<type_t>&& return_type)
    : parameters_(std::move(parameters)), return_type_(std::move(return_type))
{
}

mutable_t::mutable_t(safe_ptr<type_t>&& base_type) : base_type_(std::move(base_type)) {}

}