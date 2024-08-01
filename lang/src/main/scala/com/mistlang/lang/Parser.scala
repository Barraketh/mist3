package com.mistlang.lang

import com.mistlang.lang.Ast._
import fastparse._

trait Parser {
  def parse(s: String): Program
}

object FastparseParser extends Parser {
  override def parse(s: String): Program = {
    val grammar = new Grammar
    fastparse.parse(s, grammar.program(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure =>
        val t = f.trace()
        throw new RuntimeException(t.longAggregateMsg)

    }
  }
}

class Grammar {
  import Grammar._
  import SingleLineWhitespace._

  var id = 0
  def nextId(): Int = {
    id += 1
    id
  }

  def str[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(s => Literal(nextId(), s))

  def int[_: P] = P(CharsWhileIn("0-9", 1).!).map(s => Literal(nextId(), s.toInt))

  def bool[_: P] = P("true" | "false").!.map(s => Literal(nextId(), s.toBoolean))

  def alphaName[_: P] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.filter(s => !keyWords.contains(s))

  def symbol[_: P] = P(CharsWhile(c => symbolChars.contains(c), 1)).!.filter(s => !keySymbols.contains(s))

  def name[_: P] = P(alphaName | symbol)

  def ident[_: P] = name.map(s => Ident(nextId(), s))

  def literal[_: P]: P[TypeExpr] = P(str | int | bool)

  def term[_: P]: P[Expr] = P(literal | ifP | block | ident)

  def ifP[_: P]: P[Expr] = P("if" ~/ "(" ~ expr ~ ")" ~ "\n".? ~ expr ~ "\n".? ~ "else" ~ "\n".? ~ expr).map {
    case (cond, succ, fail) => If(nextId(), cond, succ, fail)
  }

  def funcApply[_: P]: P[Trailer] =
    P("(" ~/ ("\n".rep ~ expr ~ "\n".rep).rep(0, ",") ~ ")").map(args => FuncApply(args.toList))

  def memberRef[_: P]: P[TypeTrailer] = P("." ~/ name).map(n => MemberRefTrailer(n))

  def infixCall[_: P]: P[Trailer] = P(symbol ~/ expr).map { case (s, e) =>
    InfixCall(s, e)
  }

  def trailer[_: P]: P[Trailer] = P(memberRef | funcApply | infixCall)

  def expr[_: P]: P[Expr] = P(term ~ trailer.rep).map { case (e, items) =>
    items.foldLeft(e)((curExpr, tralier) =>
      tralier match {
        case MemberRefTrailer(name) => Ast.MemberRef(nextId(), curExpr, name)
        case FuncApply(args)        => Call(nextId(), curExpr, args)
        case InfixCall(op, arg) =>
          arg match {
            case c @ Call(_, Ident(_, otherOp), otherArgs, true) if {
                  (for {
                    opPriority <- precedenceMap.get(op)
                    otherPriority <- precedenceMap.get(otherOp)
                  } yield opPriority > otherPriority).getOrElse(false)
                } =>
              Call(
                nextId(),
                Ident(nextId(), otherOp),
                List(Call(nextId(), Ident(nextId(), op), List(curExpr, otherArgs.head)), otherArgs(1))
              )
            case _ => Call(nextId(), Ident(nextId(), op), List(curExpr, arg), isInfixCall = true)
          }

      }
    )
  }

  def typeApply[_: P]: P[TypeApply] =
    P("[" ~/ ("\n".rep ~ typeExpr ~ "\n".rep).rep(1, ",") ~ "]").map(args => TypeApply(args.toList))

  def typeTrailer[_: P]: P[TypeTrailer] = P(memberRef | typeApply)

  def typeTerm[_: P]: P[TypeExpr] = P(literal | ident)

  def typeExpr[_: P]: P[TypeExpr] = (typeTerm ~ typeTrailer.rep(0, ",")).map { case (e, items) =>
    items.foldLeft(e) { (curExp, trailer) =>
      trailer match {
        case TypeApply(args)              => Call(nextId(), curExp, args)
        case MemberRefTrailer(memberName) => Ast.MemberRef(nextId(), curExp, memberName)
      }
    }
  }

  def valP[_: P] = P("val " ~/ name ~ "=" ~ expr).map { case (n, e) => Val(n, e) }
  def defP[_: P] = P("def " ~/ name ~ argDeclList ~/ (":" ~ typeExpr) ~/ ("=" ~ expr)).map {
    case (name, args, outputType, body) => Def(Lambda(nextId(), Some(name), args, Some(outputType), body))
  }

  def topLevelBlock[_: P]: P[List[TopLevelStmt]] = P("{") ~ topLevelStmts ~ "}"

  def struct[_: P] = P("struct" ~/ name ~ typeArgList.? ~ argDeclList ~ topLevelBlock.?).map {
    case (name, typeArgs, args, stmts) =>
      Struct(name, typeArgs.getOrElse(Nil), args, stmts.getOrElse(Nil))
  }

  def argDecl[_: P] = P(name ~/ ":" ~ typeExpr).map(ArgDecl.tupled)

  def argDeclList[_: P] = P("(" ~/ argDecl.rep(0, ",") ~ ")").map(_.toList)

  def typeArgList[_: P] = P("[" ~/ argDecl.rep(min = 1, sep = ",") ~ "]").map(_.toList)

  def stmt[_: P] = P(valP | expr)

  def block[_: P] = P("{" ~/ stmts ~ "}").map(s => Block(nextId(), s))

  def stmts[_: P] = P("\n".rep ~ stmt.rep(0, "\n".rep(1)) ~ "\n".rep).map(_.toList)

  def namespace[_: P] = (P("namespace") ~/ name ~ "{" ~ topLevelStmts ~ "}").map(Namespace.tupled)

  def topLevelStmt[_: P] = namespace | struct | defP

  def topLevelStmts[_: P]: P[List[TopLevelStmt]] =
    P("\n".rep ~ topLevelStmt.rep(0, "\n".rep(1)) ~ "\n".rep).map(_.toList)

  def program[_: P]: P[Program] = P(topLevelStmts ~ stmts ~ End).map(Program.tupled)
}

object Grammar {
  sealed trait Trailer
  sealed trait TypeTrailer extends Trailer
  case class TypeApply(args: List[Expr]) extends TypeTrailer
  case class MemberRefTrailer(memberName: String) extends TypeTrailer

  case class FuncApply(args: List[Expr]) extends Trailer

  case class InfixCall(name: String, arg: Expr) extends Trailer

  val keyWords = List("true", "false")
  val keySymbols = List("=", "=>")
  val symbolChars = "+-*/|<>=@"

  val precedenceMap = Map("+" -> 0, "-" -> 0, "*" -> 1, "/" -> 1)
}
