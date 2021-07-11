#pragma once

#include <llvm/IR/Value.h>
#include <string>
#include <vector>

namespace lang {
using namespace std;

enum class TypeEnum { DOUBLE, INTEGER, /*STRING,*/ BOOL };

class CodeGenContext;

class Node {
public:
    virtual llvm::Value *code_gen(CodeGenContext &context) { return nullptr; }
    virtual ~Node() {}
};

class Expression : public Node {};

class Statement : public Node {};

class Identifier : public Expression {
public:
    Identifier(string *name) : name_(name) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    const string *name() const { return name_; }

private:
    string *name_;
};

class Arg : public Node {
public:
    Arg(TypeEnum type, Identifier *ident) : type_(type), ident_(ident) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    TypeEnum type() const { return type_; }
    Identifier *ident() const { return ident_; }

private:
    TypeEnum type_;
    Identifier *ident_ = nullptr;
};

class Args : public Node {
public:
    Args() {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    void push(Arg *arg) { args_.push_back(arg); }
    const vector<Arg *> &args() const { return args_; }

private:
    vector<Arg *> args_;
};

class ReturnType : public Node {
public:
    ReturnType(TypeEnum type) : type_(type) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    TypeEnum type() const { return type_; }

private:
    TypeEnum type_;
};

class Stmts : public Node {
public:
    Stmts() {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    void push(Statement *stmt) { stmts_.push_back(stmt); }
    const vector<Statement *> &stmts() const { return stmts_; }

private:
    vector<Statement *> stmts_;
};

class InitDecl : public Statement {
public:
    InitDecl(Identifier *ident) : ident_(ident) {}
    InitDecl(Identifier *ident, Expression *expr)
        : ident_(ident), expr_(expr) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Identifier *ident() const { return ident_; }
    Expression *expr() const { return expr_; }

private:
    Identifier *ident_ = nullptr;
    Expression *expr_ = nullptr;
};

class InitDecls : public Statement {
public:
    InitDecls() {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    void push(InitDecl *init) { inits_.push_back(init); }
    const vector<InitDecl *> &inits() const { return inits_; }

private:
    vector<InitDecl *> inits_;
};

class VarDecl : public Statement {
public:
    VarDecl(TypeEnum type, InitDecls *inits) : type_(type), inits_(inits) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    TypeEnum type() const { return type_; }
    InitDecls *inits() const { return inits_; }

private:
    TypeEnum type_;
    InitDecls *inits_ = nullptr;
};

class VarDecls : public Statement {
public:
    VarDecls() {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    void push(VarDecl *var) { vars_.push_back(var); }
    const vector<VarDecl *> &vars() const { return vars_; }

private:
    vector<VarDecl *> vars_;
};

class CompoundStmts : public Statement {
public:
    CompoundStmts(VarDecls *vars, Stmts *stmts) : vars_(vars), stmts_(stmts) {}
    CompoundStmts(VarDecls *vars) : vars_(vars) {}
    CompoundStmts(Stmts *stmts) : stmts_(stmts) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    VarDecls *vars() const { return vars_; }
    Stmts *stmts() const { return stmts_; }

private:
    VarDecls *vars_ = nullptr;
    Stmts *stmts_ = nullptr;
};

class Func : public Node {
public:
    Func(Identifier *ident, Args *args, ReturnType *ret, CompoundStmts *cs)
        : ident_(ident), args_(args), ret_(ret), cs_(cs) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Identifier *ident() const { return ident_; }
    Args *args() const { return args_; }
    ReturnType *ret_type() const { return ret_; }
    CompoundStmts *compound_stmts() const { return cs_; }

private:
    Identifier *ident_ = nullptr;
    Args *args_ = nullptr;
    ReturnType *ret_ = nullptr;
    CompoundStmts *cs_ = nullptr;
};

class PActuals : public Expression {
public:
    PActuals() {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    void push(Expression *expr) { exprs_.push_back(expr); }
    const vector<Expression *> exprs() const { return exprs_; }

private:
    vector<Expression *> exprs_;
};

class Actuals : public Expression {
public:
    Actuals(PActuals *act) : args_(act) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    PActuals *args() const { return args_; }

private:
    PActuals *args_ = nullptr;
};

class CallExpr : public Expression {
public:
    CallExpr(Identifier *ident, Actuals *args) : ident_(ident), args_(args) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Identifier *ident() const { return ident_; }
    Actuals *args() const { return args_; }

private:
    Identifier *ident_ = nullptr;
    Actuals *args_ = nullptr;
};

class AssignStmt : public Statement {
public:
    AssignStmt(Identifier *ident, Expression *expr)
        : ident_(ident), expr_(expr) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Identifier *ident() const { return ident_; }
    Expression *expr() const { return expr_; }

private:
    Identifier *ident_ = nullptr;
    Expression *expr_ = nullptr;
};

class CallStmt : public Statement {
public:
    CallStmt(CallExpr *func) : func_(func) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    CallExpr *func() const { return func_; }

private:
    CallExpr *func_ = nullptr;
};

class ReturnStmt : public Statement {
public:
    ReturnStmt(Expression *expr) : expr_(expr) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Expression *expr() const { return expr_; }

private:
    Expression *expr_ = nullptr;
};

class BreakStmt : public Statement {
public:
    virtual llvm::Value *code_gen(CodeGenContext &context);
};

class ContinueStmt : public Statement {
public:
    virtual llvm::Value *code_gen(CodeGenContext &context);
};

class IfStmt : public Statement {
public:
    IfStmt(Expression *test_expr, Stmts *if_stmts)
        : test_expr_(test_expr), if_stmts_(if_stmts) {}
    IfStmt(Expression *test_expr, Stmts *if_stmts, Stmts *else_stmts)
        : test_expr_(test_expr), if_stmts_(if_stmts), else_stmts_(else_stmts) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Expression *test_expr() const { return test_expr_; }
    Stmts *if_stmts() const { return if_stmts_; }
    Stmts *else_stmts() const { return else_stmts_; }

private:
    Expression *test_expr_ = nullptr;
    Stmts *if_stmts_ = nullptr;
    Stmts *else_stmts_ = nullptr;
};

class WhileStmt : public Statement {
public:
    WhileStmt(Expression *test_expr, Stmts *stmts)
        : test_expr_(test_expr), stmts_(stmts) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Expression *test_expr() const { return test_expr_; }
    Stmts *stmts() const { return stmts_; }

private:
    Expression *test_expr_ = nullptr;
    Stmts *stmts_ = nullptr;
};

class TrueExpr : public Expression {
public:
    virtual llvm::Value *code_gen(CodeGenContext &context);
};

class FalseExpr : public Expression {
public:
    virtual llvm::Value *code_gen(CodeGenContext &context);
};

// class StringExpr : public Expression {
// public:
//     StringExpr(string *value) : value_(value) {}
//     virtual llvm::Value *code_gen(CodeGenContext &context);

//     const string *value() const { return value_; }

// private:
//     string *value_;
// };

class DoubleExpr : public Expression {
public:
    DoubleExpr(double value) : value_(value) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    double value() const { return value_; }

private:
    double value_;
};

class IntegerExpr : public Expression {
public:
    IntegerExpr(int64_t value) : value_(value) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    int64_t value() const { return value_; }

private:
    int64_t value_;
};

class NotExpr : public Expression {
public:
    NotExpr(Expression *expr) : expr_(expr) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Expression *expr() const { return expr_; }

private:
    Expression *expr_ = nullptr;
};

class NegExpr : public Expression {
public:
    NegExpr(Expression *expr) : expr_(expr) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    Expression *expr() const { return expr_; }

private:
    Expression *expr_ = nullptr;
};

enum class OP {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Greater,
    Less,
    GEQ,
    LEQ,
    EQQ,
    NEQ,
    OrQ,
    AndQ
};

class OpExpr : public Expression {
public:
    OpExpr(Expression *left, OP op, Expression *right)
        : left_(left), op_(op), right_(right) {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    OP op() const { return op_; }
    Expression *left() const { return left_; }
    Expression *right() const { return right_; }

private:
    OP op_;
    Expression *left_ = nullptr;
    Expression *right_ = nullptr;
};

class Program : public Node {
public:
    Program() {}
    virtual llvm::Value *code_gen(CodeGenContext &context);

    void push(Func *func) { funcs_.push_back(func); }
    const vector<Func *> &func() const { return funcs_; }

private:
    vector<Func *> funcs_;
};

} // namespace lang
