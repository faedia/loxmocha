rm -rf ./build/coverage_files/
mkdir -p ./build/coverage_files/
LLVM_PROFILE_FILE="`pwd`/build/coverage_files/cov_%m.profraw" ctest --preset linux-clang-debug-coverage
llvm-profdata merge -sparse ./build/coverage_files/*.profraw -o ./build/merged.profdata
llvm-cov show ./build/linux-clang-debug-coverage/test/ast/loxmocha_ast_test -object=./build/linux-clang-debug-coverage/test/lexer/loxmocha_lexer_test -instr-profile=./build/merged.profdata -format=html -output-dir=./build/coverage_report -show-line-counts-or-regions -show-branches=count --Xdemangler=c++filt --show-branch-summary --show-instantiations=false -show-directory-coverage -ignore-filename-regex="test/.*" 