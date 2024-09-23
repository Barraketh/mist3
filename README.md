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

## Some ideas that this language will explore

### Type checker as an interpreter

There is an obvious correspondence between code written at the value level and at the type level. For example,
```scala
def foo(a: Int, b: String): String = append(toString(a), b)

val a = 3
val b = "bar"
foo(a, b)
```

could very easily translate to 
```scala
def foo(a, b) = {
  assert(a == String)
  assert(b == Int)
  val res = append(toString(a), b)
  assert(res == String)
  res
}

val a = Int
val b = String
foo(a, b)
```

Executing the second program will typecheck the original program. By doing typechecking in this way, we can in theory
make the typechecking fast (by using techniques to make interpreters fast). It also makes integration of compile-time
evaluation relatively simple. See more [here](docs/Typechecking.md)

### Explicit code specialization

The compiler will automatically optimize code based on variables known at compile time. However, it may also make sense
to specialize functions at runtime, if we know them to execute in a tight loop. For instance

```scala
def f1(a: Int, b: String) = if (a < 3) "Foo " + b else "Bar " + b
val f2 = specialize(f, a=2) // f2 = (b: String) => "Foo" + b
```
Notice that in this case f2 does not contain the 'if' statement from f1. This would give us most of the advantages of JIT,
but explicitly under the programmer's control