#pragma once

#include "loxmocha/node.hpp"

#include <string_view>

namespace loxmocha {

using node_base_t = std::string_view;

template<typename... Kinds>
class ast_node_t : public node_t<node_base_t, Kinds...> {
public:
    using node_t<node_base_t, Kinds...>::node_t;
};

} // namespace loxmocha
