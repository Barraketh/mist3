package com.mistlang.java.stdlib;

public class Functions {

  public interface Function0<Out> {
    Out apply();
  }

  static <Out> Function0<Out> f0(Function0<Out> f) {
    return f;
  }

  public interface VFunction0 {
    void apply();
  }

  static VFunction0 vf0(VFunction0 f) {
    return f;
  }


  public interface Function1<A, Out> {
    Out apply(A _0);
  }

  static <A, Out> Function1<A, Out> f1(Function1<A, Out> f) {
    return f;
  }

  public interface VFunction1<A> {
    void apply();
  }

  static <A> VFunction1<A> vf1(VFunction1<A> f) {
    return f;
  }


  public interface Function2<A, B, Out> {
    Out apply(A _0, B _1);
  }

  static <A, B, Out> Function2<A, B, Out> f2(Function2<A, B, Out> f) {
    return f;
  }

  public interface VFunction2<A, B> {
    void apply();
  }

  static <A, B> VFunction2<A, B> vf2(VFunction2<A, B> f) {
    return f;
  }


  public interface Function3<A, B, C, Out> {
    Out apply(A _0, B _1, C _2);
  }

  static <A, B, C, Out> Function3<A, B, C, Out> f3(Function3<A, B, C, Out> f) {
    return f;
  }

  public interface VFunction3<A, B, C> {
    void apply();
  }

  static <A, B, C> VFunction3<A, B, C> vf3(VFunction3<A, B, C> f) {
    return f;
  }


  public interface Function4<A, B, C, D, Out> {
    Out apply(A _0, B _1, C _2, D _3);
  }

  static <A, B, C, D, Out> Function4<A, B, C, D, Out> f4(Function4<A, B, C, D, Out> f) {
    return f;
  }

  public interface VFunction4<A, B, C, D> {
    void apply();
  }

  static <A, B, C, D> VFunction4<A, B, C, D> vf4(VFunction4<A, B, C, D> f) {
    return f;
  }

}