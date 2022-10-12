package com.mistlang.lang

import com.mistlang.lang.Ast._
import fastparse._

trait Parser {
  def parse(s: String): List[Ast]
}

object FastparseParser extends Parser {
  override def parse(s: String): List[Ast] = {
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

  def str[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(s => Literal(s))

  def int[_: P] = P(CharsWhileIn("0-9", 1).!).map(s => Literal(s.toInt))

  def bool[_: P] = P("true" | "false").!.map(s => Literal(s.toBoolean))

  def alphaName[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!

  def symbol[_: P] = P(CharsWhile(c => symbolChars.contains(c), 1)).!

  def name[_: P] = P(alphaName | symbol).filter(s => !keyWords.contains(s))

  def ident[_: P] = name.map(s => Ident(s))

  def tuple[_: P] = P("#[" ~/ expr.rep(0, ",") ~ "]").map(l => Tuple(l.toList))

  def term[_: P]: P[Expr] = P(str | int | bool | tuple | lambda | ifP | ident | block)

  def ifP[_: P]: P[Expr] = P("if" ~/ "(" ~ expr ~ ")" ~ "\n".? ~ expr ~ "\n".? ~ "else" ~ "\n".? ~ expr).map {
    case (cond, succ, fail) => If(cond, succ, fail)
  }

  def trailer[_: P]: P[Seq[Expr]] = P("(" ~/ ("\n".rep ~ expr ~ "\n".rep).rep(0, ",") ~ ")")

  def expr[_: P]: P[Expr] = P(term ~ trailer.rep).map { case (e, items) =>
    items.foldLeft(e)((f, args) => Call(f, args.toList))
  }

  def valP[_: P] = P("val" ~ name ~ "=" ~ expr).map { case (n, e) => Val(n, e) }

  def argDecl[_: P] = P(name ~/ ":" ~ expr).map(ArgDecl.tupled)

  def argDeclList[_: P] = P("(" ~ argDecl.rep(0, ",") ~ ")").map(_.toList)

  def funcData[_: P] = P(argDeclList ~ (":" ~ expr).? ~ ("=>" | "=") ~ expr)
  def lambda[_: P] = P("fn" ~ funcData).map { case (args, outType, body) =>
    Lambda(args, outType, body, None)
  }
  def defP[_: P] = P("def" ~ name ~ funcData).map { case (name, (args, outType, body)) =>
    Def(name, Lambda(args, outType, body, Some(name)))
  }

  def stmt[_: P] = P(valP | defP | expr)

  def stmtWP[_: P] = P(stmt ~ "\n".rep)

  def block[_: P] = P("{" ~/ stmts ~ "}").map(stmts => Block(stmts))

  def stmts[_: P] = P("\n".rep ~ stmtWP.rep).map(_.toList)

  def allStmts[_: P] = P(stmts ~ End)
}
