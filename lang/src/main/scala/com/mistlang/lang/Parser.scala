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
  val keyWords = List("true", "false")
  val keySymbols = List("=", "=>")
  val symbolChars = "+-*/|<>=@"

  val precedenceMap = Map("+" -> 0, "-" -> 0, "*" -> 1, "/" -> 1)

  import SingleLineWhitespace._

  def str[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(s => Literal(s))

  def int[_: P] = P(CharsWhileIn("0-9", 1).!).map(s => Literal(s.toInt))

  def bool[_: P] = P("true" | "false").!.map(s => Literal(s.toBoolean))

  def alphaName[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.filter(s => !keyWords.contains(s))

  def symbol[_: P] = P(CharsWhile(c => symbolChars.contains(c), 1)).!.filter(s => !keySymbols.contains(s))

  def name[_: P] = P(alphaName | symbol)

  def ident[_: P] = name.map(s => Ident(s))

  def tuple[_: P] = P("#[" ~/ expr.rep(0, ",") ~ "]").map(l => Tuple(l.toList))

  def term[_: P]: P[Expr] = P(str | int | bool | tuple | lambda | ifP | ident | block)

  def ifP[_: P]: P[Expr] = P("if" ~/ "(" ~ expr ~ ")" ~ "\n".? ~ expr ~ "\n".? ~ "else" ~ "\n".? ~ expr).map {
    case (cond, succ, fail) => If(cond, succ, fail)
  }

  def funcApply[_: P]: P[Trailer] =
    P("(" ~/ ("\n".rep ~ expr ~ "\n".rep).rep(0, ",") ~ ")").map(args => FuncApply(args.toList))

  def infixCall[_: P]: P[Trailer] = P(symbol ~/ expr).map { case (s, e) =>
    InfixCall(s, e)
  }

  def trailer[_: P]: P[Trailer] = P(funcApply | infixCall)

  def expr[_: P]: P[Expr] = P(term ~ trailer.rep).map { case (e, items) =>
    items.foldLeft(e)((curExpr, tralier) =>
      tralier match {
        case FuncApply(args) => Call(curExpr, args)
        case InfixCall(op, arg) =>
          arg match {
            case Call(Ident(otherOp), otherArgs, true) if {
                  (for {
                    opPriority <- precedenceMap.get(op)
                    otherPriority <- precedenceMap.get(otherOp)
                  } yield opPriority > otherPriority).getOrElse(false)
                } =>
              Call(Ident(otherOp), List(Call(Ident(op), List(curExpr, otherArgs.head)), otherArgs(1)))
            case _ => Call(Ident(op), List(curExpr, arg), isInfixCall = true)
          }

      }
    )
  }

  def valP[_: P] = P("val" ~/ name ~ "=" ~ expr).map { case (n, e) => Val(n, e) }

  def argDecl[_: P] = P(name ~/ ":" ~ expr).map(ArgDecl.tupled)

  def argDeclList[_: P] = P("(" ~/ argDecl.rep(0, ",") ~ ")").map(_.toList)

  def funcData[_: P] = P(argDeclList ~ (":" ~ expr).? ~ ("=>" | "=") ~ expr)
  def lambda[_: P] = P("fn" ~/ funcData).map { case (args, outType, body) =>
    Lambda(args, outType, body, None)
  }
  def defP[_: P] = P("def" ~/ name ~/ funcData).map { case (name, (args, outType, body)) =>
    Def(name, Lambda(args, outType, body, Some(name)))
  }

  def stmt[_: P] = P(valP | defP | expr)

  def block[_: P] = P("{" ~/ stmts ~ "}").map(stmts => Block(stmts))

  def stmts[_: P] = P("\n".rep ~ stmt.rep(0, "\n".rep(1)) ~ "\n".rep).map(_.toList)

  def allStmts[_: P] = P(stmts ~ End)

  sealed trait Trailer
  case class FuncApply(args: List[Expr]) extends Trailer
  case class InfixCall(name: String, arg: Expr) extends Trailer
}
