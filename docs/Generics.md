# Implementing Generic Functions

Consider the simple code
```scala
def render[A: Type] = fn (a: A): String => {
  if (A == Int) intToString(a)
  else if (A == String) a
  else panic("Don't know how to render " + a)
}

val a = render[Int](3)
val b = render[String]("foo")
```

One way of think of monomorphization of such functions is that a generic (or compile-time) function compiles
down to a struct. So that code would compile down to something like

```scala
val render = Dict(
  int: fn (a: Int) => intToString(a),
  string: fn (a: String) => a
)

val a = render.int(3)
val b = render.string("foo")
```

In order to implement this transformation, a compile time function must memoize its arguments and outputs. An interesting
question arises about whether these functions can be first class. Let's say we want to wrap the invocation of 'render': 
in a function
```scala
def renderWrapper(render: [A: Type] => Func[A, String]): Unit = {
  val a = render[Int](3)
  val b = render[String]("foo")
}
```
What can we compile this to? Specifically, what is the type of the 'render' parameter in the compilation output ? 
You might reasonably say that it's this:
```scala
def renderWrapper(render: Struct(int: Func[Int, String], string: Func[String, String])): Unit = {
  val a = render.int(3)
  val b = render.string("foo")
}
```
But how do we arrive at this type? This means that we must memoize the params of render not only at the original definition site,
but also at the site of the parameter. Tracking the subsequent calls at param level also allows us to correctly update the
caller of renderWrapper.