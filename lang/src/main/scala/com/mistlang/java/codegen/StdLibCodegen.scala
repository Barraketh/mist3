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
    val funcDefs = (0 until 10).map { i =>
      val inputTypes = chars.take(i).map(_.toString)
      val allTypesGeneric = s"<${(inputTypes :+ "Out").mkString(", ")}>"
      val funcName = s"Function$i$allTypesGeneric"

      val inputTypesGeneric = if (inputTypes.nonEmpty) s"<${inputTypes.mkString(", ")}>" else ""
      val vFuncName = s"VFunction$i$inputTypesGeneric"
      val args = inputTypes.zipWithIndex.map { case (t, j) => s"$t _$j" }.mkString(", ")

      s"""
     public interface $funcName {
       Out apply($args);
     }

     public interface $vFuncName {
       void apply($args);
     }
     """
    }

    s"""
   public class Functions {
     ${funcDefs.mkString("\n")}
   }
   """
  }
}
