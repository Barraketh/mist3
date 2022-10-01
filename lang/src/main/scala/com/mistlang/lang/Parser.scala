package com.mistlang.lang

import fastparse._

trait Parser {
  def parse(s: String): List[Tree[AstOp]]
}

object FastparseParser extends Parser {
  override def parse(s: String): List[Tree[AstOp]] = {
    fastparse.parse(s, Grammar.allStmts(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure =>
        val t = f.trace()
        throw new RuntimeException(t.longAggregateMsg)

    }
  }
}

object Grammar {
  val keyWords = List("true", "false", "=", "=>")
  val symbolChars = "+-*/|<>=@"

  import SingleLineWhitespace._

  def str[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(s => Leaf(AstOp.Literal(s)))

  def int[_: P] = P(CharsWhileIn("0-9", 1).!).map(s => Leaf(AstOp.Literal(s.toInt)))

  def bool[_: P] = P("true" | "false").!.map(s => Leaf(AstOp.Literal(s.toBoolean)))

  def alphaName[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!

  def symbol[_: P] = P(CharsWhile(c => symbolChars.contains(c), 1)).!

  def name[_: P] = P(alphaName | symbol).filter(s => !keyWords.contains(s))

  def ident[_: P] = name.map(s => Leaf(AstOp.Ident(s)))

  def typeExpr[_: P] = ident | tuple

  def argDecl[_: P] = P(name ~/ ":" ~ expr)

  def lambda[_: P] = P("fn" ~/ "(" ~ argDecl.rep(0, ",") ~ ")" ~ (":" ~ typeExpr).? ~ "=>" ~ expr).map {
    case (args, outType, body) =>
      Node(
        AstOp.Lambda(args.map(_._1).toList, outType.isDefined),
        args.map(_._2).toList ::: outType.toList ::: body :: Nil
      )
  }

  def tuple[_: P] = P("#[" ~/ expr.rep(0, ",") ~ "]").map(l => Node(AstOp.Tuple, l.toList))

  def term[_: P]: P[Tree[AstOp]] = P(str | int | bool | lambda | tuple | ident | block)

  def trailer[_: P]: P[Seq[Tree[AstOp]]] = P("(" ~/ ("\n".rep ~ expr ~ "\n".rep).rep(0, ",") ~ ")")

  def expr[_: P]: P[Tree[AstOp]] = P(term ~ trailer.rep).map { case (e, items) =>
    items.foldLeft(e)((f, args) => Node(AstOp.Call, f :: args.toList))
  }

  def valP[_: P] = P("val" ~ name ~ "=" ~ expr).map { case (n, e) => Node(AstOp.Val(n), e :: Nil) }

  def defP[_: P] = P("def" ~ name ~ "=" ~ expr).map { case (n, e) => Node(AstOp.Def(n), e :: Nil) }

  def stmt[_: P] = P(valP | defP | expr)

  def stmtWP[_: P] = P(stmt ~ "\n".rep)

  def block[_: P] = P("{" ~/ stmts ~ "}").map(stmts => Node(AstOp.Block, stmts))

  def stmts[_: P] = P("\n".rep ~ stmtWP.rep).map(_.toList)

  def allStmts[_: P] = P(stmts ~ End)
}
