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
    public static final Function2IntIntBool smallerOp = (a, b) -> a < b;
    public static final Function2IntIntBool eqIntOp = (a, b) -> a == b;

    public static final Functions.Function1<Object[], String> printArray = (arr) -> {
        String s = "List(";
        for (int i = 0; i < arr.length; i++) {
            s += arr[i];
            if (i < arr.length - 1) {
                s += ", ";
            }
        }
        s += ")";
        return s;
    };

    public static final Functions.Function1<Integer, Integer[]> intArrayMake = Integer[]::new;
    public static final Functions.Function2<Integer[], Integer, Integer> intArrayGet = (arr, i) -> arr[i];
    public static final Functions.Function3<Integer[], Integer, Integer, Unit> intArraySet = (arr, i, value) -> {
        arr[i] = value;
        return Unit.unit;
    };

    public static final Functions.Function1<Integer[], String> intArrayPrint = (arr) -> printArray.apply(arr);

    public static final Functions.Function1<Integer, Boolean[]> boolArrayMake = Boolean[]::new;
    public static final Functions.Function2<Boolean[], Integer, Boolean> boolArrayGet = (arr, i) -> arr[i];
    public static final Functions.Function3<Boolean[], Integer, Boolean, Unit> boolArraySet = (arr, i, value) -> {
        arr[i] = value;
        return Unit.unit;
    };
    public static final Functions.Function1<Boolean[], String> boolArrayPrint = (arr) -> printArray.apply(arr);


    public static final boolean True = true;
    public static final boolean False = false;

}
