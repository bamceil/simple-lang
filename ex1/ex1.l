%{
void printChar(char c);
%}

%%
[a-zA-Z]        printChar(yytext[0]);

#               return 0;
.               printf("%c", yytext[0]);
%%

int main(int argc, char* argv[]) {
    yylex();
    return 0;
}

void printBigChar(char c) {
    int r = c + 13;
    if (r > 'Z') r = 'A' + (r - ('Z' + 1));
    printf("%c", r);
}

void printSmallChar(char c) {
    int r = c + 13;
    if (r > 'z') r = 'a' + (r - ('z' + 1)); 
    printf("%c", r);
}

void printChar(char c) {
    if (c >= 'a' && c <= 'z') printSmallChar(c);
    else if (c >= 'A' && c <= 'Z') printBigChar(c);
    else return;
}

int yywrap(void) {
    return 1;
}
