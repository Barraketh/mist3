# mist
A small but powerful programming language to explore the limits of compile time execution

## Why compile time execution? 

1) Performance. For example, if we know the json schema that we want to parse from a string, we could generate a parser 
that is specialized to that schema.  Similarly, we can specialize database queries, etc.

2) Type safety. We can encode complex compile time checks much more easily than with standard type systems. Some examples:
    * Tensor algebra. We often know the dimensions of our tensors statically, so we can have them checked at compile time
    * Units of measure. Again, we usually know the units of our computations at compile time

3) Language simplicity. Features like generics or polymorphism can be written as libraries of Mist

## Example comptime use

### Checked matrix sizes for multiplication
```scala
class Matrix[height: Int, width: Int](data: Array[Array[Int]])

def mult(m1: Matrix, m2: Matrix): Matrix = {
  comptime(assert(m1.width == m2.height, "Width of m1 != height of m2"))
  val newData = ... // Do multiplication here
  Matrix[m1.height, m2.width](newData)
}
```

The code above will compile (on the JVM) to something like

```java
public static class Matrix(int[][] data) {}

public Matrix mult(Matrix m1, Matrix m2) {
    int[][] newData = ... // Do multiplication here
    return new Matrix(newData);
}
```



## Open questions
* How do we do syntax extensions? In an ideal world, we would do syntax extensions as part of the language, but how?
Ideally, AST rewrites, but how do they play with scoping / imports ? 

