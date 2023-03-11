package com.mistlang.java.stdlib;

public class StdFunctions {
    public interface Function2IntIntInt {
        int apply(int _0, int _1);
    }

    public interface Function2IntIntBool {
        boolean apply(int _0, int _1);
    }

    public static final Function2IntIntInt plusOp = Integer::sum;
    public static final Function2IntIntInt minusOp = (a, b) -> a - b;
    public static final Function2IntIntInt productOp = (a, b) -> a * b;
    public static final Function2IntIntBool eqIntOp = (a, b) -> a == b;

}
