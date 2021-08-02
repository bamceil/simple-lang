#include "codegen.hpp"

namespace lang {
Function *builtin_printf_function(CodeGenContext &context) {
    std::vector<Type *> printf_arg_types;
    printf_arg_types.push_back(Type::getInt8PtrTy(langContext)); // char*

    FunctionType *printf_type = FunctionType::get(Type::getInt32Ty(langContext),
                                                  printf_arg_types, true);

    Function *func = Function::Create(printf_type, Function::ExternalLinkage,
                                      Twine("printf"), context.module_);
    func->setCallingConv(CallingConv::C);
    return func;
}

void builtin_print(CodeGenContext &context, Function *printfFn,
                   const char *format_str, const char *funcName,
                   Type *argType) {
    std::vector<Type *> print_arg_types;
    print_arg_types.push_back(argType);

    FunctionType *print_func_type = FunctionType::get(
        Type::getInt64Ty(langContext), print_arg_types, false);

    Function *func =
        Function::Create(print_func_type, Function::InternalLinkage,
                         Twine(funcName), context.module_);
    BasicBlock *bblock = BasicBlock::Create(langContext, "entry", func, 0);
    Builder.SetInsertPoint(bblock);

    context.push_block(bblock);

    Constant *format_const =
        ConstantDataArray::getString(langContext, format_str);
    GlobalVariable *var = new GlobalVariable(
        *context.module_,
        ArrayType::get(IntegerType::get(langContext, 8),
                       strlen(format_str) + 1),
        true, GlobalValue::PrivateLinkage, format_const, ".str");
    Constant *zero =
        Constant::getNullValue(IntegerType::getInt32Ty(langContext));

    std::vector<Constant *> indices;
    indices.push_back(zero);
    indices.push_back(zero);
    Constant *var_ref = ConstantExpr::getGetElementPtr(
        ArrayType::get(IntegerType::get(langContext, 8),
                       strlen(format_str) + 1),
        var, indices);

    std::vector<Value *> args;
    args.push_back(var_ref);

    Function::arg_iterator argsValues = func->arg_begin();
    Value *toPrint = &*argsValues++;
    toPrint->setName("toPrint");
    args.push_back(toPrint);

    CallInst *call = Builder.CreateCall(printfFn, makeArrayRef(args), "print");
    Builder.CreateRet(call);

    // CallInst *call = CallInst::Create(printfFn, makeArrayRef(args), "",
    // bblock);
    // ReturnInst::Create(langContext, bblock);
    context.pop_block();
}

void builtin_function(CodeGenContext &context) {
    Function *f = builtin_printf_function(context);
    builtin_print(context, f, "%d\n", "printi", Type::getInt64Ty(langContext));
    builtin_print(context, f, "%lf\n", "printd",
                  Type::getDoubleTy(langContext));
}

} // namespace lang