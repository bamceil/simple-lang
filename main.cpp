#include "ast.hpp"
#include "codegen.hpp"

#include <iostream>

extern int yyparse();
extern FILE *yyin;

using namespace lang;
using namespace llvm;

extern Program *prog;

int main(int argc, char **argv) {
    if (argc != 3) {
        cerr << "expected two file input.\n";
        return -1;
    }
    yyin = fopen(argv[1], "r");
    yyparse();
    fclose(yyin);

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    CodeGenContext context;
    builtin_function(context);

    context.generate_code(*prog, argv[2]);
    return 0;
}