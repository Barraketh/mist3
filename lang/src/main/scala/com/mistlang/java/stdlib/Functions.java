package com.mistlang.java.stdlib;

public class Functions {

  public interface Function0<Out> {
    Out apply();
  }
  public interface Function1<A, Out> {
    Out apply(A _0);
  }
  public interface Function2<A, B, Out> {
    Out apply(A _0, B _1);
  }
  public interface Function3<A, B, C, Out> {
    Out apply(A _0, B _1, C _2);
  }
  public interface Function4<A, B, C, D, Out> {
    Out apply(A _0, B _1, C _2, D _3);
  }
  public interface Function5<A, B, C, D, E, Out> {
    Out apply(A _0, B _1, C _2, D _3, E _4);
  }
  public interface Function6<A, B, C, D, E, F, Out> {
    Out apply(A _0, B _1, C _2, D _3, E _4, F _5);
  }
  public interface Function7<A, B, C, D, E, F, G, Out> {
    Out apply(A _0, B _1, C _2, D _3, E _4, F _5, G _6);
  }
  public interface Function8<A, B, C, D, E, F, G, H, Out> {
    Out apply(A _0, B _1, C _2, D _3, E _4, F _5, G _6, H _7);
  }

  public interface Function9<A, B, C, D, E, F, G, H, J, Out> {
    Out apply(A _0, B _1, C _2, D _3, E _4, F _5, G _6, H _7, J _8);
  }
}