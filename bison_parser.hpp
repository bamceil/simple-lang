/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_BISON_PARSER_HPP_INCLUDED
# define YY_YY_BISON_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IdentifierT = 258,
    IntConstant = 259,
    DoubleConstant = 260,
    True = 261,
    False = 262,
    Return = 263,
    While = 264,
    Function = 265,
    Arrow = 266,
    Int = 267,
    Double = 268,
    Bool = 269,
    If = 270,
    Else = 271,
    Break = 272,
    Continue = 273,
    LE = 274,
    GE = 275,
    EQ = 276,
    NE = 277,
    And = 278,
    Or = 279
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 14 "parser.y"

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

#line 114 "bison_parser.hpp"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_BISON_PARSER_HPP_INCLUDED  */
