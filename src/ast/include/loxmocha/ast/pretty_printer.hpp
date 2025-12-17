#pragma once

#include "loxmocha/ast/decl.hpp"
#include "loxmocha/ast/expr.hpp"
#include "loxmocha/ast/pattern.hpp"
#include "loxmocha/ast/stmt.hpp"
#include "loxmocha/ast/type.hpp"
#include "loxmocha/source/source.hpp"
#include "loxmocha/ast/module.hpp"

#include <ostream>
#include <vector>

namespace loxmocha {

class pretty_printer_t {
public:
    void operator()(node_base_t                     span,
                    const module::module_t&         module,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const decl::type_t&             decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const decl::function_t&         decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const decl::variable_t&         decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const decl::error_t&            decl,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::literal_t&          expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::identifier_t&       expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::binary_t&           expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void
    operator()(node_base_t span, const expr::is_t& expr, const source::source_manager_t& source, std::ostream& stream);

    void operator()(node_base_t                     span,
                    const expr::cast_t&             expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::unary_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::index_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::field_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::call_t&             expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::array_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::record_t&           expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::tuple_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::grouping_t&         expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void
    operator()(node_base_t span, const expr::if_t& expr, const source::source_manager_t& source, std::ostream& stream);

    void operator()(node_base_t                     span,
                    const expr::while_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::block_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const expr::error_t&            expr,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const pattern::identifier_t&    pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const pattern::tag_t&           pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const pattern::error_t&         pattern,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const stmt::expr_t&             stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const stmt::assign_t&           stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const stmt::decl_t&             stmt,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::identifier_t&       type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::array_t&            type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::tuple_t&            type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::record_t&           type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::tagged_t&           type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::reference_t&        type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::function_t&         type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

    void operator()(node_base_t                     span,
                    const type::mutable_t&          type,
                    const source::source_manager_t& source,
                    std::ostream&                   stream);

private:
    std::vector<bool> indent_stack_;
};

} // namespace loxmocha
