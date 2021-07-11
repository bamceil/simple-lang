bison -d -o bison_parser.cpp parser.y
flex -o flex_lang.cpp lang.l
clang++ -g -I/usr/include/llvm-10/ -I/usr/include/llvm-c-10/ `llvm-config --cxxflags --ldflags --libs` -o lang *.cpp