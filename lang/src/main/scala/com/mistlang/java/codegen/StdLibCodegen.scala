package com.mistlang.java.codegen

object StdLibCodegen {
  val chars = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'I')

  val tuplesClass = {
    val tupleDefs = (0 until 9).map { i =>
      val n = i + 1
      val types = chars.take(n)
      val params = types.zipWithIndex.map { case (t, j) => s"$t _$j" }.mkString(", ")
      s"public record Tuple$n<${types.mkString(", ")}>($params) {}"
    }
    s"""
     class Tuples {
       ${tupleDefs.mkString("\n")}
     }
     """
  }

  val functionsClass = {
    val funcDefs = (0 until 5).map { i =>
      val inputTypes = chars.take(i).map(_.toString)
      val allTypesGeneric = s"<${(inputTypes :+ "Out").mkString(", ")}>"
      val funcName = s"Function$i$allTypesGeneric"

      val inputTypesGeneric = if (inputTypes.nonEmpty) s"<${inputTypes.mkString(", ")}>" else ""
      val vFuncName = s"VFunction$i$inputTypesGeneric"

      s"""
     public interface $funcName {
       Out apply(${inputTypes.zipWithIndex.map { case (t, j) => s"$t _$j" }.mkString(", ")});
     }
     static $allTypesGeneric $funcName f$i($funcName f) { return f; }

     public interface $vFuncName {
       void apply();
     }
     static $inputTypesGeneric $vFuncName vf$i($vFuncName f) { return f; }
     """
    }

    s"""
   class Functions {
     ${funcDefs.mkString("\n")}
   }
   """
  }
}
