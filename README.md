# mist
A small but powerful programming language to explore the limits of compile time execution

## Why compile time execution? 

1) Type safety. Compile time checks apply to ALL programs generated. As we generate more and more code using LLMs, it will
become more and more important to have compile time guarantees of its correctness. The goal is to let us encode complex
compile time checks more simply. Some motivating examples:
   * Tensor algebra. We often know the dimensions of our tensors statically, so we can have them checked at compile time
   * Units of measure. Again, we usually know the units of our computations at compile time

2) Performance. For example, if we know the json schema that we want to parse from a string, we could generate a parser 
that is specialized to that schema.  Similarly, we can specialize database queries, etc.


3) Language simplicity. Features like generics or polymorphism can be written as libraries of Mist

## Example comptime use

### Checked matrix sizes for multiplication
```scala
lazy Matrix = Struct(height: comptime Int, width: comptime Int, data: Array[Array[Int]])

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


