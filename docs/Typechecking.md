# Typechecker as interprerter

The basic idea is that in order to typecheck our code we can transform it into a type-level program,
and then run the program.

## Typechecking values and functions

The simplest and most natural transformation would be from

```scala
def foo(a: Int, b: String): String = append(toString(a), b)

val c = 3
val d = "bar"
foo(c, d) // Returns "3bar"
```

To

```scala
def foo(a, b) = {
  assert(a == String)
  assert(b == Int)
  val res = append(toString(a), b)
  assert(res == String)
  res
}

val c = Int
val d = String
foo(a, b)
```

That is, transform all literals to their types, transform function type constraints to asserts, declare victory.

This broadly works, but raises a couple of questions:
1. The function `foo` does not get type checked independently. Could we make sure that the body of `foo` independently
typechecks? 
2. How do we deal with recursive functions?

Both can be resolved by changing out transformation in the following way:

```scala
// This ia a global function to typecheck a function call. We will replace every call to a function
// foo(a, b), with a call to typecheck(foo, List(a, b))
// This is also somewhat simplified - the real function would also support things like varargs, comptime modifiers,
// Etc
def typecheck(f: Type, args: List[Type]): Type = {
  f match {
    case Func(argTypes, outType) =>
      assert(args.length == argTypes.length, "Wrong number of args")
      args.zip(argTypes).foreach {
        case (arg, expectedType) => assert(arg == expectedType, "Type mismatch")
      }
      outType
    case _ => throw new Exception("Can't call " + f)
  }
}

// foo gets replaced with an instance of a function type
val foo = Func[Int, Int, String]

// Here we make sure that the body makes sense by assuming that the parameters are of the expected types.
// This also solves the 'recursion' issue - even if 'foo' was recursive originally, the transformed 'foo'
// is not
{
  val a = Int
  val b = Int
  val res = typecheck(append, List(typecheck(toString, List(a)), b))
  assert(res == String, "Type mismatch")
}

val c = Int
val d = String
typecheck(foo, List(c, d))
```

## Typechecking if statements

```scala
if (cond) a else b
```
can be transformed to

```scala
def ifFunc(a, b, c) = {
  assert(a == Bool)
  if (b == c) b
  else Any
}

ifFunc(cond, a, b)
```

Notice that although at the value level only one branch will be taken, at the type level both branches are typed

## Some general notes

- The above transformation assumes that regular functions must have return types. Technically this is only required for
recursive functions, and it would be simple to infer the types of non-recursive functions from the body. However,
I am lazy, so regular functions shall have return types.
- The execution of the 'typelevel' program will correctly typecheck our program, but it does not maintain types for use
in compiling to Java. A simple solution (and one I shall use) is to have an id for every expression, and to keep a cache
of types