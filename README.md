# Mini-Lambda-Interpreter

## 功能

### 1. 基础内容

具体见doc/fpproject.md



### 2. 代数数据类型（ADT）的支持

包括代数数据类型的声明、代数数据类型的构造函数、模式匹配语句的支持。



### 3. 设计文法、实现Parser

文法：

```haskell
<term> := <bool> | <int> | <char>  (example：True, 18, 'a')
		| <identifier> | (<expr>)
<identifier> := xxxx
<type> := Bool | Int | Char 
		| <identifier> -- adt type
		| <type> -> <type>
<binary_operator> := and | or | * | / | % | + | - | > | < | >= | <= | == | /=
<unary_operator> := not
<expr> := <term> <binary_operator> <term> 
		| <unary_operator> <term>
		| <if_expr>
		| <let_expr>
		| <letrec_expr>
		| <lambda_expr>
		| <case_expr>
		| <apply_expr>
		| <variable_expr>
		| (<expr>)
<if_expr> := if <expr> then <expr> else <expr>
<let_expr> := let <identifier> := <expr> in <expr>
<letrec_expr> := letrec <type> def <identifier>(<identifier :: <type>>){<expr>}
				 in <expr>
<lambda_expr> := \ <identifier> :: <type> -> <expr>
<apply_expr> := | <expr> $ <expr>
<variable_expr> := $<identifier>

<case_expr> := case <expr> of <patternAssigns>
<patternAssigns> := <patternAssign>; <patternAssign>; .. ;<patternAssign>
<patternAssign> := <patterns> --> <expr>
<patterns> := <pattern>, <pattern>, .. ,<pattern>
<pattern> := <bool> | <int> | <char> | <identifier> | <adtpattern>
<adtpattern> := [<identifier>] patterns
-- adt definition grammar
<adtDefine> := data <identifier> := <constructors>
<constructors> := <constructor> | <constructor> | .. | <constructor>
<constructor> := <identifier>(<types>)
<types> := <type> , <type>, .. , <type>
```

实现：基于megaparsec，代码位于MiniParser.hs文件中，。



### 4. 简单的REPL

支持的语句：

```haskell
-- adt definition, same as MiniParser
-- example: 
-- data List := Cons (Int->Int, List->(Int->Int)) | Nil ()
<adtDefine>

-- bind
-- example:
-- let x = (let x = 3 in x)
let <identifier> = <expr>

-- eval type of an expression
-- example:
-- :t let x = 3 in x
:t <expr>

-- eval value of an expression
-- example:
-- eval let x = 3 in x
eval <expr>

-- multi-lines block
-- example1:
-- :{
-- data List := Cons (Int->Int, List->(Int->Int)) 
--			| Nil ()
-- :}
-- example2:
-- :{
-- let x = 
-- 3 in x			
-- :}
:{
<expr>
:}

-- show all binds in current context
:bind

-- show command information
:help

-- exit
:quit
```

实现：

ReplParser.hs：在MiniParser的基础上补充了有关REPL的语法

ReplContext.hs：维护了REPL的上下文信息，包括ADT的定义以及表达式绑定

MiniRepl.hs：不断读取输入，进行相应的操作



### 5. JavaScript编译器

支持所有基本语言特性。

实现：核心思想是将if、lambda表达式和let表达式处理为一个函数，通过函数闭包实现多层作用域，外层函数中需要定义内层函数，并利用内层函数计算表达式。具体代码在GenCode.hs中。



## 运行

Parser：

1. 直接运行MiniParser中的run函数，输入为一行字符串，输出为错误信息或者表达式；运行MiniParser中的runAdtDefine函数，输入为一行字符串，输出为错误信息或者Adt的定义表达式
2. 在REPL下进行测试

测试方法：

​	运行MiniRepl的main函数。

JavaScript编译器：

1. 运行GenCode.hs中的runGen方法，输入Expr类型的值以及输出文件路径，通过node.js运行输出文件即可
2. 运行GenCode.hs中的main方法，输入MiniParser的合法语句，输出在同级目录下的output.js文件中，通过node.js运行文件即可