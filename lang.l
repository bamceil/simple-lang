%{
#include <cstdlib>
#include <string>
#include "ast.hpp"
#include "bison_parser.hpp"
int cur_line = 1;
void yyerror(const char *msg);
void unrecognized_char(char c);
void unterminate_string();
%}


/* note: \042 is '"' */
WHITESPACE          ([ \t\r\a]+)
INTEGER             ([0-9]+)
DOUBLE              ([0-9]+.[0-9]+)
/*UNTERM_STRING       (\042[^\042\n]*)*/
/*STRING              (\042[^\042\n]*\042)*/
/*{STRING}            { yylval.string = new std::string(yytext); return StringConstant;  }*/
/*{UNTERM_STRING}     {  unterminate_string();   }*/
IDENTIFIER          ([_a-zA-Z][_a-zA-Z0-9]*)
OPERATOR            ([+*-/%=,;!<>(){}])
COMMENT             ("//"[^\n]*)

/* "string"            { return String;          } */

%%

[\n]                { cur_line++;           }
{WHITESPACE}        { /* ignore all spaces */   }
{COMMENT}           { /* return Comment; */         }


{OPERATOR}          { return yytext[0];         }

"<="                { return LE;              }
">="                { return GE;              }
"=="                { return EQ;              }
"!="                { return NE;              }
"&&"                { return And;             }
"||"                { return Or;              }
"->"                { return Arrow;           }

"int"               { return Int;             }
"double"            { return Double;          }
"bool"              { return Bool;            }

"true"              { return True;            }
"false"             { return False;           }
"function"          { return Function;        }    
"while"             { return While;           }
"if"                { return If;              }
"else"              { return Else;            }
"return"            { return Return;          }
"break"             { return Break;           }
"continue"          { return Continue;        }


{INTEGER}           { yylval.number = atoll(yytext); return IntConstant;     }
{IDENTIFIER}        { yylval.string = new std::string(yytext); return IdentifierT;      }
{DOUBLE}            { yylval.real = atof(yytext); return DoubleConstant;  }

.                   {  unrecognized_char(yytext[0]);   }

%%

void unrecognized_char(char c) {
    char buf[32] = "Unrecognized character: ?";
    buf[24] = c;
    yyerror(buf);
}

void unterminate_string() {
    yyerror("Unterminate string constant");
}

int yywrap(void) {
    return 1;
}

void yyerror(const char *msg) {
    fprintf(stderr, "Error at line %d:\n\t%s\n", cur_line, msg);
    exit(-1);
}