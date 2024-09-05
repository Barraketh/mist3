package com.mistlang.lang

import com.mistlang.lang.Ast._
import fastparse._

object FastparseParser {
  def defaultIdProivder: () => Option[Int] = {
    var id = 0

    def nextId(): Option[Int] = {
      id += 1
      Some(id)
    }
    nextId
  }

  def parse(s: String, nextId: () => Option[Int] = defaultIdProivder): Program = {
    val grammar = new Grammar(nextId)
    fastparse.parse(s, grammar.program(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure =>
        val t = f.trace()
        throw new RuntimeException(t.longAggregateMsg)

    }
  }

  def parseExpr(s: String, nextId: () => Option[Int] = defaultIdProivder): Expr = {
    val grammar = new Grammar(nextId)
    fastparse.parse(s, grammar.fullExpr(_)) match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure =>
        val t = f.trace()
        throw new RuntimeException(t.longAggregateMsg)
    }
  }

}

class Grammar(nextId: () => Option[Int]) {
  import Grammar._
  import SingleLineWhitespace._

  def str[_: P] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(s => Literal(nextId(), s))

  def int[_: P] = P(CharsWhileIn("0-9", 1).!).map(s => Literal(nextId(), s.toInt))

  def bool[_: P] = P("true" | "false").!.map(s => Literal(nextId(), s.toBoolean))

  def alphaName[_: P] = P(CharIn("a-zA-Z_$") ~~ CharsWhileIn("a-zA-Z0-9_$", 0)).!.filter(s => !keyWords.contains(s))

  def symbol[_: P] = P(CharsWhile(c => symbolChars.contains(c), 1)).!.filter(s => !keySymbols.contains(s)).map {
    case "+"   => "plusOp"
    case "-"   => "minusOp"
    case "*"   => "productOp"
    case "<"   => "smallerOp"
    case "=="  => "eqIntOp"
    case other => other
  }

  def name[_: P] = P(alphaName | symbol)

  def ident[_: P] = name.map(s => Ident(nextId(), s))

  def literal[_: P]: P[TypeExpr] = P(str | int | bool)

  def term[_: P]: P[Expr] = P(literal | ifP | lambda | block | ident)

  def ifP[_: P]: P[Expr] = P("if" ~/ "(" ~ expr ~ ")" ~ "\n".? ~ expr ~ "\n".? ~ "else" ~ "\n".? ~ expr).map {
    case (cond, succ, fail) => If(nextId(), cond, succ, fail)
  }

  def applyArgs[_: P]: P[List[Expr]] = P("(" ~/ ("\n".rep ~ expr ~ "\n".rep).rep(0, ",") ~ ")").map(_.toList)

  def funcApply[_: P]: P[Trailer] = applyArgs.map(args => FuncApply(args))

  def memberRef[_: P]: P[TypeTrailer] = P("." ~/ name).map(n => MemberRefTrailer(n))

  def methodCall[_: P]: P[TypeTrailer] = P("::" ~/ name ~/ applyArgs).map { case (name, args) =>
    MethodCall(name, args)
  }

  def infixCall[_: P]: P[Trailer] = P(symbol ~/ expr).map { case (s, e) =>
    InfixCall(s, e)
  }

  def trailer[_: P]: P[Trailer] = P(memberRef | methodCall | funcApply | typeApply | infixCall)

  def expr[_: P]: P[Expr] = P(term ~ trailer.rep).map { case (e, items) =>
    items.foldLeft(e)((curExpr, tralier) =>
      tralier match {
        case MemberRefTrailer(name) => Ast.MemberRef(nextId(), curExpr, name)
        case MethodCall(name, args) => Call(nextId(), Ast.MethodRef(nextId(), curExpr, name), curExpr :: args)
        case FuncApply(args)        => Call(nextId(), curExpr, args)
        case TypeApply(args)        => Call(nextId(), curExpr, args)
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

  def lambda[_: P] = (P("fn ") ~/ argDeclList ~ (":" ~ typeExpr).? ~/ ("=>" ~ expr)).map {
    case (args, outputType, body) => Lambda(nextId(), None, args, outputType, body)
  }
  def defP[_: P] = P("def " ~/ name ~ argDeclList ~/ (":" ~ typeExpr) ~/ ("=" ~ expr)).map {
    case (name, args, outputType, body) => List(Def(Lambda(nextId(), Some(name), args, Some(outputType), body)))
  }

  def topLevelBlock[_: P]: P[List[TopLevelStmt]] = P("{") ~ topLevelStmts ~ "}"

  def struct[_: P]: P[List[TopLevelStmt]] = P("struct" ~/ name ~ typeArgList.? ~ argDeclList ~ topLevelBlock.?).map {
    case (name, typeArgs, args, stmts) =>
      val structDecl = Struct(name, typeArgs.getOrElse(Nil), args)
      val namespaceDecl = stmts match {
        case Some(value) => Namespace(name + "$", value) :: Nil
        case None        => Nil
      }
      structDecl :: namespaceDecl
  }

  def argDecl[_: P] = P(name ~/ ":" ~ typeExpr).map(ArgDecl.tupled)

  def argDeclList[_: P] = P("(" ~/ argDecl.rep(0, ",") ~ ")").map(_.toList)

  def typeArgList[_: P] = P("[" ~/ argDecl.rep(min = 1, sep = ",") ~ "]").map(_.toList)

  def stmt[_: P] = P(valP | expr)

  def block[_: P] = P("{" ~/ stmts ~ "}").map(s => Block(nextId(), s))

  def stmts[_: P] = P("\n".rep ~ stmt.rep(0, "\n".rep(1)) ~ "\n".rep).map(_.toList)

  def namespace[_: P] = (P("namespace") ~/ name ~ "{" ~ topLevelStmts ~ "}").map(Namespace.tupled).map(n => List(n))

  def topLevelStmt[_: P]: P[List[TopLevelStmt]] = namespace | struct | defP

  def topLevelStmts[_: P]: P[List[TopLevelStmt]] =
    P("\n".rep ~ topLevelStmt.rep(0, "\n".rep(1)) ~ "\n".rep).map(_.toList.flatten)

  def fullExpr[_: P]: P[Expr] = P(expr ~ End)
  def program[_: P]: P[Program] = P(topLevelStmts ~ stmts ~ End).map(Program.tupled)
}

object Grammar {
  sealed trait Trailer
  sealed trait TypeTrailer extends Trailer
  case class TypeApply(args: List[Expr]) extends TypeTrailer
  case class MemberRefTrailer(memberName: String) extends TypeTrailer
  case class MethodCall(methodName: String, args: List[Expr]) extends TypeTrailer

  case class FuncApply(args: List[Expr]) extends Trailer

  case class InfixCall(name: String, arg: Expr) extends Trailer

  val keyWords = List("true", "false")
  val keySymbols = List("=", "=>")
  val symbolChars = "+-*/|<>=@"

  val precedenceMap = Map("plusOp" -> 0, "minusOp" -> 0, "productOp" -> 1, "/" -> 1)
}
