# Mini-Lambda-Interpreter



#### 运行

Parser：

1. 直接运行MiniParser中的run函数，输入为一行字符串，输出为错误信息或者表达式；运行MiniParser中的runAdtDefine函数，输入为一行字符串，输出为错误信息或者Adt的定义表达式
2. 在REPL下进行测试

测试方法：

​	运行MiniRepl的main函数。

JavaScript编译器：

1. 运行GenCode.hs中的runGen方法，输入Expr类型的值以及输出文件路径，通过node.js运行输出文件即可
2. 运行GenCode.hs中的main方法，输入MiniParser的合法语句，输出在同级目录下的output.js文件中，通过node.js运行文件即可