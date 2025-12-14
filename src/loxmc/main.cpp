#include "loxmocha/ast/lexer.hpp"
#include "loxmocha/ast/parser.hpp"
#include "loxmocha/ast/pretty_printer.hpp"
#include "loxmocha/memory/safe_pointer.hpp"
#include "loxmocha/source/source.hpp"

#include <CLI/CLI.hpp>
#include <chrono>
#include <exception>
#include <filesystem>
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

    std::filesystem::path source_file{};
    app.add_option("-f,--file", source_file, "Source file")->required();

    CLI11_PARSE(app, argc, argv);

    loxmocha::source::source_manager_t source_manager{};

    try {
        std::string source_content = read_file(source_file);
        auto        source_info =
            loxmocha::safe_ptr<loxmocha::source::source_info_t>::make(source_file, std::move(source_content));
        source_manager.emplace(std::move(source_info));

        loxmocha::lexer_t lexer(source_manager.find_source(source_file).view().content());
        auto start_time = std::chrono::high_resolution_clock::now();
        auto              parse_result = loxmocha::parse_decl(lexer);
        auto end_time   = std::chrono::high_resolution_clock::now();

        std::println("Parsing took {} microseconds",
                     std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count());

        if (!parse_result) {
            std::println(std::cerr, "Parsing failed with errors:");
            for (const auto& diag : parse_result.diagnostics()) {
                std::println(std::cerr, "- {}", diag);
            }
            return 1;
        }

        std::println("Parsing succeeded.");
        std::println("Printing AST:");
        parse_result.result().visit(loxmocha::pretty_printer_t{}, source_manager, std::cout);

    } catch (const std::exception& e) {
        std::println(std::cerr, "Error: {}", e.what());
        return 1;
    }

    return 0;
}
