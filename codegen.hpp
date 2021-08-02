#pragma once

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <typeinfo>
#include <vector>

namespace lang {
using namespace llvm;
using namespace std;

class Program;

static LLVMContext langContext;
static IRBuilder<> Builder(langContext);

struct CodeGenBlock {
    BasicBlock *block_;
    map<std::string, Value *> locals_;

    CodeGenBlock() : CodeGenBlock(nullptr) {}
    CodeGenBlock(BasicBlock *b) : block_(b) {}
};

class CodeGenContext {
public:
    Module *module_;

    CodeGenContext() : module_(new Module("main", langContext)) {}

    void generate_code(Program &root, const string &file);
    GenericValue run_code();
    map<string, Value *> &locals() { return blocks_.back()->locals_; }
    BasicBlock *current_block() { return blocks_.back()->block_; }
    void push_block(BasicBlock *block) {
        blocks_.push_back(new CodeGenBlock(block));
    }
    void pop_block() {
        CodeGenBlock *top = blocks_.back();
        blocks_.pop_back();
        delete top;
    }

    Value *find_symbol(const string &name);

    vector<BasicBlock *> &break_list() { return break_list_; }

    vector<BasicBlock *> &continue_list() { return continue_list_; }

private:
    vector<CodeGenBlock *> blocks_;
    Function *main_;
    vector<BasicBlock *> break_list_;
    vector<BasicBlock *> continue_list_;
};

void builtin_function(CodeGenContext &context);
} // namespace lang