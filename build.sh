bison -vdty parser.y
flex lang.l
g++ -g -o lang y.tab.c lex.yy.c codegen.cpp