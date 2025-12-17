#include "loxmocha/ast/module.hpp"

#include "loxmocha/ast/base.hpp"
#include "loxmocha/ast/decl.hpp"

#include <utility>
#include <vector>

namespace loxmocha::module {

module_t::module_t(const node_base_t& base, std::vector<decl::decl_t>&& declarations)
    : base_{base}, declarations_(std::move(declarations))
{
}

} // namespace loxmocha::module
