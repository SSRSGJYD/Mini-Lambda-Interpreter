### 函数式语言程序设计 实验报告

软件62 周展平 2016013253

------

[TOC]

#### 功能

##### 1. 必做部分

具体见fpproject.md



##### 2. 代数数据类型（ADT）的支持

包括代数数据类型的声明、代数数据类型的构造函数、模式匹配语句的支持。



##### 3. 设计文法、实现Parser

文法：

```haskell
<term> := <bool> | <int> | <char>  (example：true, 18, 'a')
		| <identifier> | (<expr>)
<identifier> := xxxx
<type> := Bool | Int | Char 
		| <identifier> -- for adt
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
<pattern> := True | False 
```

