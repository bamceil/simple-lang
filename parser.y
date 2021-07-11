
%{
#include <string>
#include "ast.hpp"

void yyerror(const char*);

extern int yylex();

lang::Program *prog;

%}

%union {
    double real;
    std::string *string;
    int64_t number;
    lang::Program* program;
    lang::Expression* expr;
    lang::Statement* stmt;
    lang::Stmts* stmts;
    lang::Identifier* ident;
    lang::Node* node;
    lang::Func* func;
    lang::Args* args;
    lang::TypeEnum type;
    lang::ReturnType* ret;
    lang::CompoundStmts* compstmts;
    lang::VarDecl* var;
    lang::VarDecls* vars;
    lang::InitDecl* init;
    lang::InitDecls* inits;
    lang::PActuals* pactual;
    lang::Actuals* actuals;
    lang::CallExpr* callexpr;
    lang::CallStmt* callstmt;
    lang::ReturnStmt* ret_stmt;
    lang::BreakStmt* break_stmt;
    lang::AssignStmt* assign_stmt;
    lang::ContinueStmt* continue_stmt;
    lang::IfStmt* if_stmt;
    lang::WhileStmt* while_stmt;
    // String StringConstant
}

%token <string> IdentifierT
%token <number> IntConstant  
%token <real> DoubleConstant
%token <string> True False Return While Function Arrow Int Double Bool
%token <string> If Else Break Continue LE GE EQ NE
%token <string> And Or

%type <ident> Identifier
%type <expr> Expr TestExpr
%type <program> Program
%type <func> Func
%type <args> Args _Args
%type <type> Type
%type <ret> ReturnDecl
%type <stmts> Stmts
%type <stmt> Stmt
%type <compstmts> CompoundStmts
%type <var> VarDecl
%type <vars> VarDecls
%type <init> InitDecl
%type <inits> InitDecls
%type <pactual> PActuals
%type <actuals> Actuals
%type <callexpr> CallExpr
%type <assign_stmt> AssignStmt
%type <callstmt> CallStmt
%type <ret_stmt> ReturnStmt
%type <break_stmt> BreakStmt
%type <continue_stmt> ContinueStmt
%type <stmts> StmtsBlock
%type <if_stmt> IfStmt
%type <while_stmt> WhileStmt

%left '='
%left Or
%left And
%left EQ NE
%left '<' '>' LE GE
%left '+' '-'
%left '*' '/' '%'
%left '!'

%start Program

%%

Program:
    /* empty */             { prog = new lang::Program(); $$ = prog;  }
|   Program Func            { $1->push($2); }
;

Func:
    Function Identifier '(' Args ')' ReturnDecl CompoundStmts 
        { $$ = new lang::Func($2, $4, $6, $7); }
;

ReturnDecl:
    /* empty */             { $$ = nullptr; }
|   Arrow Type              { $$ = new lang::ReturnType($2); }
;

Type:
    Double                 { $$ = lang::TypeEnum::DOUBLE; }
|   Int                    { $$ = lang::TypeEnum::INTEGER; }
|   Bool                   { $$ = lang::TypeEnum::BOOL; }
;

/*|   String                 { $$ = lang::TypeEnum::STRING; }*/

Identifier:
    IdentifierT              { $$ = new lang::Identifier($1); }
;

Args:
    /* empty */             { $$ = nullptr; }
|   _Args                   { $$ = $1; }
;

_Args:
    Type Identifier                  { auto p = new lang::Args(); p->push(new lang::Arg($1, $2)); $$ = p; }
|   _Args ',' Type Identifier        { $1->push(new lang::Arg($3, $4)); }
;

CompoundStmts:
    '{' Stmts '}'               { $$ = new lang::CompoundStmts($2); }
|   '{' VarDecls '}'            { $$ = new lang::CompoundStmts($2); }
|   '{' VarDecls Stmts '}'      { $$ = new lang::CompoundStmts($2, $3); }
;

VarDecls:
    VarDecls VarDecl        { $1->push($2); }
|   VarDecl                 { $$ = new lang::VarDecls(); $$->push($1); }
;

VarDecl:
    Type InitDecls ';'              { $$ = new lang::VarDecl($1, $2); }
;

InitDecls:
    InitDecl                        { $$ = new lang::InitDecls(); $$->push($1); }
|   InitDecls ',' InitDecl          { $1->push($3); }
;

InitDecl:
    Identifier                  { $$ = new lang::InitDecl($1); }
|   Identifier '=' Expr         { $$ = new lang::InitDecl($1, $3); }
;

Stmts:
    Stmts Stmt              { $1->push($2); }
|   Stmt                    { $$ = new lang::Stmts(); $$->push($1); }
;

Stmt:
    CompoundStmts           { $$ = $1; }
|   AssignStmt              { $$ = $1; }
|   CallStmt                { $$ = $1; }
|   ReturnStmt              { $$ = $1; }
|   IfStmt                  { $$ = $1; }
|   WhileStmt               { $$ = $1; }
|   BreakStmt               { $$ = $1; }
|   ContinueStmt            { $$ = $1; }
;

AssignStmt:
    Identifier '=' Expr ';'      { $$ = new lang::AssignStmt($1, $3); }
;

CallStmt:
    CallExpr ';'            { $$ = new lang::CallStmt($1); }
;

CallExpr:
    Identifier '(' Actuals ')'       { $$ = new lang::CallExpr($1, $3); }
;

Actuals:
    /* empty */             { $$ = nullptr; }
|   Expr PActuals           { $2->push($1); $$ = new lang::Actuals($2); }
;

PActuals:
    /* empty */             { $$ = new lang::PActuals(); }
|   PActuals ',' Expr       { $1->push($3); }
;

ReturnStmt:
    Return Expr ';'       { $$ = new lang::ReturnStmt($2); }
|   Return ';'            { $$ = new lang::ReturnStmt(nullptr); }
;

IfStmt:
    If TestExpr StmtsBlock                          { $$ = new lang::IfStmt($2, $3); }
|   If TestExpr StmtsBlock Else StmtsBlock          { $$ = new lang::IfStmt($2, $3, $5); }
;

TestExpr:
    '(' Expr ')'            { $$ = $2; }
;

StmtsBlock:
    '{' Stmts '}'           { $$ = $2; }
;

WhileStmt:
    While TestExpr StmtsBlock        { $$ = new lang::WhileStmt($2, $3); }
;

BreakStmt:
    Break ';'     { $$ = new lang::BreakStmt(); }
;

ContinueStmt:
    Continue ';'  { $$ = new lang::ContinueStmt(); }
;

Expr:
    Expr '+' Expr           { $$ = new lang::OpExpr($1, lang::OP::Add, $3); }
|   Expr '-' Expr           { $$ = new lang::OpExpr($1, lang::OP::Sub, $3); }
|   Expr '*' Expr           { $$ = new lang::OpExpr($1, lang::OP::Mul, $3); }
|   Expr '/' Expr           { $$ = new lang::OpExpr($1, lang::OP::Div, $3); }
|   Expr '%' Expr           { $$ = new lang::OpExpr($1, lang::OP::Mod, $3); }
|   Expr '>' Expr           { $$ = new lang::OpExpr($1, lang::OP::Greater, $3); }
|   Expr '<' Expr           { $$ = new lang::OpExpr($1, lang::OP::Less, $3); }
|   Expr GE Expr            { $$ = new lang::OpExpr($1, lang::OP::GEQ, $3); }
|   Expr LE Expr            { $$ = new lang::OpExpr($1, lang::OP::LEQ, $3); }
|   Expr EQ Expr            { $$ = new lang::OpExpr($1, lang::OP::EQQ, $3); }
|   Expr NE Expr            { $$ = new lang::OpExpr($1, lang::OP::NEQ, $3); }
|   Expr Or Expr            { $$ = new lang::OpExpr($1, lang::OP::OrQ, $3); }
|   Expr And Expr           { $$ = new lang::OpExpr($1, lang::OP::AndQ, $3); }
|   '-' Expr %prec '!'      { $$ = new lang::NegExpr($2); }
|   '!' Expr                { $$ = new lang::NotExpr($2); }
|   IntConstant             { $$ = new lang::IntegerExpr($1); }
|   DoubleConstant          { $$ = new lang::DoubleExpr($1); }
|   Identifier              { $$ = $1; }
|   CallExpr                { $$ = $1; }
|   '(' Expr ')'            { $$ = $2; }
|   True                    { $$ = new lang::TrueExpr(); }
|   False                   { $$ = new lang::FalseExpr(); }
;

/*|   StringConstant          { $$ = new lang::StringExpr($1); }*/

%%

