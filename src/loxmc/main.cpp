#include "loxmocha/ast/d2_export.hpp"
#include "loxmocha/ast/dot_export.hpp"
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
    std::chrono::high_resolution_clock::time_point start_time;
    std::chrono::high_resolution_clock::time_point end_time;
    CLI::App                                       app{"LoxMocha Compiler"};

    std::filesystem::path source_file{};
    app.add_option("-f,--file", source_file, "Source file")->required();
    std::filesystem::path dot_file{};
    app.add_option("-d,--dot", dot_file, "Output DOT file for AST");
    std::filesystem::path d2_file{};
    app.add_option("--d2", d2_file, "Output D2 file for AST");

    CLI11_PARSE(app, argc, argv);

    loxmocha::source::source_manager_t source_manager{};

    try {
        start_time                 = std::chrono::high_resolution_clock::now();
        std::string source_content = read_file(source_file);
        end_time                   = std::chrono::high_resolution_clock::now();
        std::println("File reading took {} microseconds",
                     std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count());
        start_time = std::chrono::high_resolution_clock::now();
        auto source_info =
            loxmocha::safe_ptr<loxmocha::source::source_info_t>::make(source_file, std::move(source_content));
        end_time = std::chrono::high_resolution_clock::now();
        std::println("Source info creation took {} microseconds",
                     std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count());
        start_time = std::chrono::high_resolution_clock::now();
        source_manager.emplace(std::move(source_info));
        end_time = std::chrono::high_resolution_clock::now();
        std::println("Source registration took {} microseconds",
                     std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count());

        loxmocha::lexer_t lexer(source_manager.find_source(source_file).view().content());
        start_time        = std::chrono::high_resolution_clock::now();
        auto parse_result = loxmocha::parse_decl(lexer);
        end_time          = std::chrono::high_resolution_clock::now();
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

        if (!dot_file.empty()) {
            std::ofstream dot_stream{dot_file};
            if (!dot_stream.is_open()) {
                throw std::runtime_error{"Could not open DOT file for writing: " + dot_file.string()};
            }

            start_time = std::chrono::high_resolution_clock::now();
            loxmocha::dot::dot_exporter_t::export_ast(parse_result.result(), source_manager, dot_stream);
            dot_stream.close();
            end_time = std::chrono::high_resolution_clock::now();
            std::println("DOT export took {} microseconds",
                         std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count());
        }

        if (!d2_file.empty()) {
            std::ofstream d2_stream{d2_file};
            if (!d2_stream.is_open()) {
                throw std::runtime_error{"Could not open D2 file for writing: " + d2_file.string()};
            }

            start_time = std::chrono::high_resolution_clock::now();
            loxmocha::d2::d2_exporter_t::export_ast(parse_result.result(), source_manager, d2_stream);
            d2_stream.close();
            end_time = std::chrono::high_resolution_clock::now();
            std::println("D2 export took {} microseconds",
                         std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time).count());
        }

    } catch (const std::exception& e) {
        std::println(std::cerr, "Error: {}", e.what());
        return 1;
    }

    return 0;
}
