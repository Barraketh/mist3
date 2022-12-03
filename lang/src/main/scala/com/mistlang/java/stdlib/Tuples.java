package com.mistlang.java.stdlib;

public class Tuples {
  public record Tuple1<A>(A _0) {
  }

  public record Tuple2<A, B>(A _0, B _1) {
  }

  public record Tuple3<A, B, C>(A _0, B _1, C _2) {
  }

  public record Tuple4<A, B, C, D>(A _0, B _1, C _2, D _3) {
  }

  public record Tuple5<A, B, C, D, E>(A _0, B _1, C _2, D _3, E _4) {
  }

  public record Tuple6<A, B, C, D, E, F>(A _0, B _1, C _2, D _3, E _4, F _5) {
  }

  public record Tuple7<A, B, C, D, E, F, G>(A _0, B _1, C _2, D _3, E _4, F _5, G _6) {
  }

  public record Tuple8<A, B, C, D, E, F, G, H>(A _0, B _1, C _2, D _3, E _4, F _5, G _6, H _7) {
  }

  public record Tuple9<A, B, C, D, E, F, G, H, J>(A _0, B _1, C _2, D _3, E _4, F _5, G _6, H _7, J _8) {
  }

  public static class Tuple {
    public static <A> Tuple1<A> apply(A a) {
      return new Tuple1<>(a);
    }

    public static <A, B> Tuple2<A, B> apply(A a, B b) {
      return new Tuple2<>(a, b);
    }

    public static <A, B, C> Tuple3<A, B, C> apply(A a, B b, C c) {
      return new Tuple3<>(a, b, c);
    }

    public static <A, B, C, D> Tuple4<A, B, C, D> apply(A a, B b, C c, D d) {
      return new Tuple4<>(a, b, c, d);
    }

    public static <A, B, C, D, E> Tuple5<A, B, C, D, E> apply(A a, B b, C c, D d, E e) {
      return new Tuple5<>(a, b, c, d, e);
    }

    public static <A, B, C, D, E, F> Tuple6<A, B, C, D, E, F> apply(A a, B b, C c, D d, E e, F f) {
      return new Tuple6<>(a, b, c, d, e, f);
    }

    public static <A, B, C, D, E, F, G> Tuple7<A, B, C, D, E, F, G> apply(A a, B b, C c, D d, E e, F f, G g) {
      return new Tuple7<>(a, b, c, d, e, f, g);
    }

    public static <A, B, C, D, E, F, G, H> Tuple8<A, B, C, D, E, F, G, H> apply(A a, B b, C c, D d, E e, F f, G g, H h) {
      return new Tuple8<>(a, b, c, d, e, f, g, h);
    }

    public static <A, B, C, D, E, F, G, H, J> Tuple9<A, B, C, D, E, F, G, H, J> apply(A a, B b, C c, D d, E e, F f, G g, H h, J j) {
      return new Tuple9<>(a, b, c, d, e, f, g, h, j);
    }

  }
}
