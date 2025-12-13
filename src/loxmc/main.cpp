#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/source/source.hpp"

#include <CLI/CLI.hpp>
#include <exception>
#include <fstream>
#include <iostream>
#include <iterator>
#include <print>
#include <string>
#include <utility>

namespace {
auto read_file(const std::string& path) -> std::string
{
    std::ifstream file_stream{path};

    if (!file_stream.is_open()) {
        throw std::runtime_error{"Could not open file: " + path};
    }

    std::string content{std::istreambuf_iterator<char>(file_stream), std::istreambuf_iterator<char>()};
    return content;
}
} // namespace

auto main(int argc, char** argv) -> int
{
    CLI::App app{"LoxMocha Compiler"};

    std::string source_file{};
    app.add_option("-f,--file", source_file, "Source file")->required();

    CLI11_PARSE(app, argc, argv);

    loxmocha::source::source_manager_t source_manager{};

    try {
        std::string source_content = read_file(source_file);
        auto        source_info =
            loxmocha::safe_ptr<loxmocha::source::source_info_t>::make(source_file, std::move(source_content));
        source_manager.emplace(std::move(source_info));

        std::println("Loaded source file: {}", source_file);

        for (const auto& source_view : source_manager) {
            std::println("File in manager: {}", source_view.filepath().string());
        }

        auto source_view = *source_manager.find_source(std::filesystem::path{source_file});

        loxmocha::lexer_t lexer{source_manager.find_source(std::filesystem::path{source_file}).view().content()};

        auto parse_result = loxmocha::parse_decl(lexer);
        if (!parse_result) {
            for (const auto& error : parse_result.diagnostics()) {
                std::println(std::cerr, "Parse Error: {}", error);
            }
            return 1;
        }

        auto offset = source_view.content().find("var");
        auto loc    = source_view.find_location(source_view.content().substr(offset));

        if (loc) {
            std::println("Source location found: {}:{}:{}", loc->line, loc->column, source_view.filepath().string());
            auto source_view_2 = source_manager.find_source(loc->line_span).view();
            std::println("Find source by substring span: {}", source_view_2.filepath().string());
        } else {
            std::println("Source location not found.");
        }

        std::string not_registered_span = "this span does not exist";
        auto        not_found_view =
            source_manager.find_source(std::string_view{not_registered_span.begin(), not_registered_span.end()});

        if (not_found_view == source_manager.end()) {
            std::println("Correctly did not find source for unregistered span.");
        } else {
            std::println("Error: Found source for unregistered span.");
        }

        std::println("Parsing completed successfully.");

    } catch (const std::exception& e) {
        std::println(std::cerr, "Error: {}", e.what());
        return 1;
    }

    return 0;
}
