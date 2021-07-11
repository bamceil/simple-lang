#include "codegen.hpp"
#include "ast.hpp"
#include <iostream>

namespace lang {

void CodeGenContext::generate_code(Program &root, const std::string &file) {
    clog << "Generating code...\n";

    FunctionType *main_type =
        FunctionType::get(Type::getVoidTy(langContext), false);
    main_ = Function::Create(main_type, GlobalValue::ExternalLinkage, "main");
    BasicBlock *bblock = BasicBlock::Create(langContext, "entry");

    push_block(bblock);
    root.code_gen(*this);
    ReturnInst::Create(langContext, nullptr, bblock);
    pop_block();

    clog << "Generating code... End.\n";

    legacy::PassManager pm;
    pm.add(createPrintModulePass(outs()));
    pm.run(*module_);

    error_code err_info;
    llvm::raw_ostream *out =
        new llvm::raw_fd_ostream(file, err_info, sys::fs::F_None);
    llvm::WriteBitcodeToFile(*module_, *out);
    out->flush();
    delete out;
}

GenericValue CodeGenContext::run_code() {
    clog << "run code...\n";

    ExecutionEngine *ee = EngineBuilder(unique_ptr<Module>(module_)).create();
    ee->finalizeObject();
    vector<GenericValue> noargs;
    GenericValue v = ee->runFunction(main_, noargs);

    clog << "code run done.\n";
    return v;
}

Value *CodeGenContext::find_symbol(const string &name) {
    for (auto iter = blocks_.rbegin(); iter != blocks_.rend(); ++iter) {
        auto it = (*iter)->locals_.find(name);
        if (it != (*iter)->locals_.end()) { return it->second; }
    }
    return nullptr;
}

static Type *value_type(TypeEnum type) {
    switch (type) {
    case TypeEnum::BOOL:
        return Type::getInt1Ty(langContext);
    case TypeEnum::INTEGER:
        return Type::getInt64Ty(langContext);
    case TypeEnum::DOUBLE:
        return Type::getDoubleTy(langContext);
        // case TypeEnum::STRING:
        //     return Type::getInt1Ty(langContext);
    }
    cerr << "unknown type.\n";
    return nullptr;
}

llvm::Value *Program::code_gen(CodeGenContext &context) {
    std::clog << "Creating program.\n";

    for (auto func : funcs_)
        func->code_gen(context);
    return nullptr;
}

llvm::Value *Identifier::code_gen(CodeGenContext &context) {
    std::clog << "Creating identifier: " << name_->c_str() << "\n";

    Value *it = context.find_symbol(*name_);
    if (it == nullptr) {
        cerr << "unknown variable name: " << name_->c_str() << "\n";
        return nullptr;
    }
    // return iter->second;
    return Builder.CreateLoad(it, name_->c_str());
}

llvm::Value *Arg::code_gen(CodeGenContext &context) {
    clog << "Creating arg.\n";

    Value *alloc =
        Builder.CreateAlloca(value_type(type_), 0, *(ident_->name()));
    context.locals().insert(make_pair(*(ident_->name()), alloc));
    return alloc;
}

llvm::Value *Args::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *ReturnType::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *Stmts::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *InitDecl::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *InitDecls::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *VarDecl::code_gen(CodeGenContext &context) {
    clog << "Creating var declare.\n";

    Type *type = value_type(type_);
    for (auto v : inits()->inits()) {
        const string &vname = *(v->ident()->name());
        if (context.locals().find(vname) != context.locals().end()) {
            cerr << "varible " << vname << " already exist."
                 << "\n";
            return nullptr;
        }
        AllocaInst *pi = Builder.CreateAlloca(type, 0, vname.c_str());
        context.locals()[vname] = pi;
        if (v->expr() != nullptr) {
            Value *pv = v->expr()->code_gen(context);
            Builder.CreateStore(pv, pi);
        }
    }
    return nullptr;
}

llvm::Value *VarDecls::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *Func::code_gen(CodeGenContext &context) {
    clog << "Creating function: " << ident_->name()->c_str() << "\n";

    if (context.locals().find(*(ident_->name())) != context.locals().end()) {
        cerr << "function: redefined symbol " << ident_->name()->c_str()
             << "\n";
        return nullptr;
    }

    vector<Type *> argTypes;
    if (args_ != nullptr) {
        auto end = args_->args().end();
        for (auto iter = args_->args().begin(); iter != end; ++iter)
            argTypes.push_back(value_type((*iter)->type()));
    }
    FunctionType *funcType =
        ret_ == nullptr ? FunctionType::get(Type::getVoidTy(langContext),
                                            makeArrayRef(argTypes), false)
                        : FunctionType::get(value_type(ret_->type()),
                                            makeArrayRef(argTypes), false);
    Function *func = Function::Create(funcType, GlobalValue::ExternalLinkage,
                                      ident_->name()->c_str(), context.module_);

    BasicBlock *block = BasicBlock::Create(langContext, "entry", func);
    Builder.SetInsertPoint(block);

    context.push_block(block);

    if (args_ != nullptr) {
        auto ori_arg = args_->args().begin();
        for (auto &arg : func->args()) {
            arg.setName((*ori_arg)->ident()->name()->c_str());
            Value *alloc = (*ori_arg)->code_gen(context);
            Builder.CreateStore(&arg, alloc, false);
            ++ori_arg;
        }
    }

    cs_->code_gen(context);

    context.pop_block();

    context.locals()[*(ident_->name())] = func;
    return func;
}

llvm::Value *CompoundStmts::code_gen(CodeGenContext &context) {
    clog << "Creating compound statements.\n";
    if (vars_ != nullptr) {
        for (auto var : vars_->vars())
            var->code_gen(context);
    }
    if (stmts_ != nullptr) {
        for (auto stmt : stmts_->stmts())
            stmt->code_gen(context);
    }
    return nullptr;
}

llvm::Value *PActuals::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *Actuals::code_gen(CodeGenContext &context) { return nullptr; }

llvm::Value *CallExpr::code_gen(CodeGenContext &context) {
    clog << "Creating function call: " << *(ident_->name()) << "\n";

    Function *function = context.module_->getFunction(*(ident_->name()));
    if (function == nullptr) {
        cerr << "no function: " << *(ident_->name()) << "\n";
        return nullptr;
    }
    std::vector<Value *> args;
    if (args_ == nullptr) {
        if (function->arg_size() != 0) {
            cerr << "paramter not equal: " << 0
                 << " expected: " << function->arg_size() << "\n";
            return nullptr;
        }
        return Builder.CreateCall(function, args, "calltmp");
    }

    if (function->arg_size() != args_->args()->exprs().size()) {
        cerr << "paramter not equal: " << args_->args()->exprs().size()
             << " expected: " << function->arg_size() << "\n";
        return nullptr;
    }

    for (auto expr : args_->args()->exprs()) {
        args.push_back(expr->code_gen(context));
    }

    return Builder.CreateCall(function, args, "calltmp");
}

llvm::Value *AssignStmt::code_gen(CodeGenContext &context) {
    const string &name = *(ident_->name());
    clog << "Creating assignment for: " << name << "\n";

    Value *v = expr_->code_gen(context);
    if (v == nullptr) {
        cerr << "right side is unassigned."
             << "\n";
        return nullptr;
    }
    Value *vit = context.find_symbol(name);
    if (vit == nullptr) {
        cerr << "undeclared variable: " << name << "\n";
        return nullptr;
    }
    return Builder.CreateStore(v, vit);
}

llvm::Value *CallStmt::code_gen(CodeGenContext &context) {
    clog << "Creating call stmt.\n";
    return func_->code_gen(context);
}

llvm::Value *ReturnStmt::code_gen(CodeGenContext &context) {
    clog << "Creating return\n";

    Value *v = expr_ == nullptr ? nullptr : expr_->code_gen(context);

    if (v == nullptr) return Builder.CreateRetVoid();
    return Builder.CreateRet(v);
}

llvm::Value *BreakStmt::code_gen(CodeGenContext &context) {
    clog << "Creating break.\n";

    if (context.break_list().empty()) {
        cerr << "break not in loop.\n";
        return nullptr;
    }

    return Builder.CreateBr(context.break_list().back());
}

llvm::Value *ContinueStmt::code_gen(CodeGenContext &context) {
    clog << "Creating continue.\n";

    if (context.continue_list().empty()) {
        cerr << "continue not in loop.\n";
        return nullptr;
    }

    return Builder.CreateBr(context.continue_list().back());
}

static Value *cast_to_boolean(Value *condValue) {
    if (condValue->getType()->isIntegerTy()) {
        condValue = Builder.CreateIntCast(condValue,
                                          Type::getInt1Ty(langContext), true);
        return Builder.CreateICmpNE(
            condValue, ConstantInt::get(Type::getInt1Ty(langContext), 0, true));
    } else if (condValue->getType()->isDoubleTy()) {
        return Builder.CreateFCmpONE(
            condValue, ConstantFP::get(langContext, APFloat(0.0)));
    } else {
        return condValue;
    }
}

llvm::Value *IfStmt::code_gen(CodeGenContext &context) {
    clog << "Creating if-then-[else] code.\n";

    Value *condV = test_expr_->code_gen(context);
    if (condV == nullptr) {
        cerr << "if test expr gen failed.\n";
        return nullptr;
    }
    condV = cast_to_boolean(condV);

    Function *func = Builder.GetInsertBlock()->getParent();

    BasicBlock *then_block = BasicBlock::Create(langContext, "then", func);
    BasicBlock *after_if = BasicBlock::Create(langContext, "after_if");
    BasicBlock *else_block = nullptr;

    if (else_stmts_ != nullptr) {
        else_block = BasicBlock::Create(langContext, "else");
        Builder.CreateCondBr(condV, then_block, else_block);
    } else {
        Builder.CreateCondBr(condV, then_block, after_if);
    }
    Builder.SetInsertPoint(then_block);

    context.push_block(then_block);
    for (auto stmt : if_stmts_->stmts())
        stmt->code_gen(context);
    context.pop_block();

    then_block = Builder.GetInsertBlock();
    if (then_block->getTerminator() == nullptr) Builder.CreateBr(after_if);

    if (else_stmts_ != nullptr) {
        func->getBasicBlockList().push_back(else_block);
        Builder.SetInsertPoint(else_block);

        context.push_block(else_block);
        for (auto stmt : else_stmts_->stmts())
            stmt->code_gen(context);
        context.pop_block();

        Builder.CreateBr(after_if);
    }

    func->getBasicBlockList().push_back(after_if);
    Builder.SetInsertPoint(after_if);
    return nullptr;
}

llvm::Value *WhileStmt::code_gen(CodeGenContext &context) {
    clog << "Creating while code.\n";

    Function *func = Builder.GetInsertBlock()->getParent();
    BasicBlock *befor_loop =
        BasicBlock::Create(langContext, "before_loop", func);
    BasicBlock *loop_block = BasicBlock::Create(langContext, "loop", func);
    BasicBlock *after_block =
        BasicBlock::Create(langContext, "after_loop", func);

    Builder.CreateBr(befor_loop);

    Builder.SetInsertPoint(befor_loop);
    Value *test = test_expr_->code_gen(context);
    if (!test) {
        cerr << "while test failed.\n";
        return nullptr;
    }
    if (test->getType()->isDoubleTy())
        test = Builder.CreateFCmpONE(
            test, ConstantFP::get(langContext, APFloat(0.0)), "whilefcond");
    else
        test = Builder.CreateICmpNE(
            test, ConstantInt::get(langContext, APInt(1, 0)), "whilecond");

    Builder.CreateCondBr(test, loop_block, after_block);

    Builder.SetInsertPoint(loop_block);

    context.break_list().push_back(after_block);
    context.continue_list().push_back(befor_loop);
    context.push_block(loop_block);
    for (auto stmt : stmts_->stmts())
        stmt->code_gen(context);
    context.pop_block();
    context.continue_list().pop_back();
    context.break_list().pop_back();

    Builder.CreateBr(befor_loop);

    Builder.SetInsertPoint(after_block);

    return nullptr;
}

llvm::Value *TrueExpr::code_gen(CodeGenContext &context) {
    clog << "Creating true\n";

    return ConstantInt::get(langContext, APInt(1, 1));
}

llvm::Value *FalseExpr::code_gen(CodeGenContext &context) {
    clog << "Creating false.\n";

    return ConstantInt::get(langContext, APInt(1, 0));
}
// llvm::Value *StringExpr::code_gen(CodeGenContext &context) { return nullptr;
// }
llvm::Value *DoubleExpr::code_gen(CodeGenContext &context) {
    clog << "Creating double: " << value_ << "\n";

    return ConstantFP::get(langContext, APFloat(value_));
}

llvm::Value *IntegerExpr::code_gen(CodeGenContext &context) {
    clog << "Creating integer: " << value_ << "\n";

    return ConstantInt::get(langContext, APInt(64, value_, true));
}

// llvm::Value *StringExpr::code_gen(CodeGenContext &context) {
//     clog << "Creating string: " << value_->c_str() << "\n";

//     return Builder.CreateGlobalString(value_->c_str(), "global_string");
// }

llvm::Value *NotExpr::code_gen(CodeGenContext &context) {
    clog << "Createing not code.\n";

    Value *v = expr_->code_gen(context);
    // if (!(v->getType()->isIntegerTy())) {
    //     cerr << "not: type error. " << endl;
    //     return nullptr;
    // }
    return Builder.CreateNot(v, "not");
}

llvm::Value *NegExpr::code_gen(CodeGenContext &context) {
    clog << "Createing neg code.\n";

    Value *v = expr_->code_gen(context);
    if (v->getType()->isDoubleTy()) return Builder.CreateFNeg(v, "neg");
    return Builder.CreateNeg(v, "neg");
}

llvm::Value *OpExpr::code_gen(CodeGenContext &context) {
    clog << "Creating operation expression.\n";

    Value *left = left_->code_gen(context);
    Value *right = right_->code_gen(context);
    if (left == nullptr || right == nullptr) return nullptr;
    bool has_float =
        left->getType()->isDoubleTy() || right->getType()->isDoubleTy();
    switch (op_) {
    case OP::Add:
        if (has_float) return Builder.CreateFAdd(left, right, "addftmp");
        return Builder.CreateAdd(left, right, "addtmp");
    case OP::Sub:
        if (has_float) return Builder.CreateFSub(left, right, "subftmp");
        return Builder.CreateSub(left, right, "subtmp");
    case OP::Mul:
        if (has_float) return Builder.CreateFMul(left, right, "mulftmp");
        return Builder.CreateMul(left, right, "multmp");
    case OP::Div:
        if (has_float) return Builder.CreateFDiv(left, right, "divftmp");
        return Builder.CreateSDiv(left, right, "divtmp");
    case OP::Mod:
        if (has_float) return Builder.CreateFRem(left, right, "remftmp");
        return Builder.CreateSRem(left, right, "remtmp");
    case OP::Greater:
        if (has_float) return Builder.CreateFCmpUGT(left, right, "gtftmp");
        return Builder.CreateICmpSGT(left, right, "gttmp");
    case OP::Less:
        if (has_float) return Builder.CreateFCmpULT(left, right, "ltftmp");
        return Builder.CreateICmpSLT(left, right, "lttmp");
    case OP::LEQ:
        if (has_float) return Builder.CreateFCmpULE(left, right, "leftmp");
        return Builder.CreateICmpSLE(left, right, "letmp");
    case OP::GEQ:
        if (has_float) return Builder.CreateFCmpUGE(left, right, "geftmp");
        return Builder.CreateICmpSGE(left, right, "getmp");
    case OP::EQQ:
        if (has_float) return Builder.CreateFCmpUEQ(left, right, "eqftmp");
        return Builder.CreateICmpEQ(left, right, "eqtmp");
    case OP::NEQ:
        if (has_float) return Builder.CreateFCmpUNE(left, right, "neftmp");
        return Builder.CreateICmpNE(left, right, "netmp");
    case OP::OrQ:
        if (has_float) {
            cerr << "double value not support OR.\n";
            return nullptr;
        }
        return Builder.CreateOr(left, right, "ortmp");
    case OP::AndQ:
        if (has_float) {
            cerr << "double value not support AND.\n";
            return nullptr;
        }
        return Builder.CreateAnd(left, right, "andtmp");
    }

    cerr << "unknown operator\n";
    return nullptr;
}

} // namespace lang