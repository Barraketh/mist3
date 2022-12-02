package com.mistlang.java.stdlib;

public class MutableRef<A> {
  public A value;

  public MutableRef(A a) {
    value = a;
  }

  public void set(A a) {
    value = a;
  }
}
