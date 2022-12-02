package com.mistlang.java.stdlib;

import java.util.HashMap;

public class StdFunctions {
  public record Entry(String key, Object value) {
  }

  public static HashMap<String, Object> dict(Entry... entries) {
    final var res = new HashMap<String, Object>();
    for (Entry entry : entries) {
      res.put(entry.key, entry.value);
    }
    return res;
  }

  public static final Functions.Function2<HashMap<String, Object>, String, Object> get = HashMap::get;
}
