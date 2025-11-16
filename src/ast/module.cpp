#include "loxmocha/ast/module.hpp"
#include "loxmocha/ast/decl.hpp"

#include <utility>
#include <vector>

namespace loxmocha::module {

module_t::module_t(std::vector<decl::decl_t>&& declarations) : declarations_(std::move(declarations)) {}

} // namespace loxmocha::module
