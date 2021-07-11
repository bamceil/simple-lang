# 语言语法定义

## 关键字

if else int double bool string byte true false return function while break continue 

## 运算符

\+ - * / % = ; , ! < > ( ) { } 

\>= <= == != && || ->

## 常量

数字： 123 123.9

字符串： "hello"

## 标识符

同 C 语言

## 注释

同 C 语言

## 示例

```csharp
function value() -> double {
    return 3.1415926;
}

function main() {  // Main function
    int a = 10, i = 0;
    string s = "hello";
    double v = value();
    
    while (i < a) {
        if (i % 2 != 0) { break; }
        continue;
    }
}

```
