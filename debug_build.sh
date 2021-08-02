bison -d -o bison_parser.cpp parser.y
flex -o flex_lang.cpp lang.l
clang++ -g -I/usr/include/llvm-10/ -I/usr/include/llvm-c-10/ -std=c++14 -o bin/lang `llvm-config --libs --cxxflags --ldflags` -lpthread -ldl -lz -lncurses -rdynamic *.cpp
