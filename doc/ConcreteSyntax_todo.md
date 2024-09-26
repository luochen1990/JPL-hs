TODO
----

-[x] 多行文本及变量嵌入
-[ ] primitive 机制支持 (derivation path 等会用到)
-[ ] Sum/Union Type
-[ ] builtins
-[ ] list index/slice syntax
-[ ] 计算字段
-[x] 自引用 (let rec)

开放式讨论问题
------------

Q: 要不要 newtype ？

目前替代 newtype 的方式:

```
Score = Int ^{ ... }    #refinement
Score = @{score: Int}   #wrap
```

但是有个进一步的问题是，当 refinement 一样的时候，要当成相同类型还是不同类型?

比如

```
Score = Int ^{check: ?x (x >= 0 and x <= 100)}
Percent = Int ^{check: ?x (x >= 0 and x <= 100)}
```

这时候 Score 和 Percent 应该被当成同一个类型还是不同类型？

更具体的问题是，以下表达式是否应该通过类型检查

```
f : ?(x: Score) (y: Score; y = x + 1; y); a : Percent = 99; f a
```

一种解决方法是，在 refinement annotation 中加一个标识字段, 如:

```
Score = Int ^{check: ?x (x >= 0 and x <= 100), id: "Score"}
Percent = Int ^{check: ?x (x >= 0 and x <= 100), id: "Percent"}
```

当然也可以不加 id 字段，直接认为两次声明的是不同的类型.


Q: 要不要支持 symbol ？

一种可能设计是，在静态分析层面， symbol 和 string 是不同的，但在运行时，它们都是 string


Q: 委派要做成静态么？ 怎么实现比较好？

讲真，还没想好
