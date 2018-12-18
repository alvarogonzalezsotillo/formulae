package formulae 

import formulae.FormulaEvaluator.{FunctionMapExpressionContext, VariableMapExpressionContext}
import formulae.FormulaParser.{BinaryExpression, Expression, ExpressionOperator, FactorOperator, FunctionCall, Number, UnaryExpression, Variable}

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers
import scala.{Unit => Void}


object FormulaEvaluator{

  trait ExpressionContext{
    def valueOf(v: Variable) : Try[Double]
    def valueOf(f: FunctionCall) : Try[Double]
  }

  trait VariableMapExpressionContext extends ExpressionContext{
    val variableMap : Map[String,Double]
    def valueOf(v: Variable ) = Try(variableMap(v.symbol.name))
  }

  trait FunctionMapExpressionContext extends ExpressionContext{
    val functionMap : Map[String,(Seq[Double])=>Double]
    def valueOf(f: FunctionCall) : Try[Double] = {
      val args = f.arguments.map( e => computeValue(e)(this) )
      val firstError = args.find( _.isFailure )
      if( firstError.isDefined ){
        firstError.get
      }
      else Try{
        val fun = functionMap(f.name.name)
        fun(args.map(_.get))
      }
    }
  }

  def computeValue(e: Expression)(implicit context: ExpressionContext) : Try[Double] = e match {
    case fc : FunctionCall => context.valueOf(fc)
    case BinaryExpression(e1, op, e2) => op.operator match {
      case "+" => for( v1 <- computeValue(e1); v2 <- computeValue(e2) ) yield v1+v2
      case "-" => for( v1 <- computeValue(e1); v2 <- computeValue(e2) ) yield v1-v2
      case "/" => for( v1 <- computeValue(e1); v2 <- computeValue(e2) ) yield v1/v2
      case "*" => for( v1 <- computeValue(e1); v2 <- computeValue(e2) ) yield v1*v2
      case _ => Failure(new Exception(s"Operador binario no reconocido: $op"))
    }
    case UnaryExpression(op, e) => op.operator match{
      case "+" => for( v <- computeValue(e) ) yield v
      case "-" => for( v <- computeValue(e) ) yield -v
      case _ => Failure(new Exception(s"Operador unario no reconocido: $op"))

    }
    case Number(value) => Success(value.toDouble)
    case v : Variable => context.valueOf(v)
    case _ => Failure( new Exception( s"Expresión no reconocida: $e") )
  }
}

object FormulaParser{


  def withIndent( out : (String)=>Void, indent: String = "  " ) = (s:String) => out( indent + s )

  trait Expression{
    def dump( out : (String) => Void = println ) : Void
  }

  case class Symbol(name:String){
    override val toString = name
  }
  trait Operator{
    val operator: String
    override val toString = operator
  }
  case class ExpressionOperator(override val operator: String) extends Operator
  case class FactorOperator(override val operator: String) extends Operator

  case class FunctionCall(name: Symbol, arguments: Seq[Expression] ) extends Expression{
    override lazy val toString = s"$name(${arguments.mkString(",")})"
    def dump( out : (String)=>Void ) = {
      out( s"Function  $name")
      arguments.foreach( _.dump(withIndent(out)))
    }
  }
  case class BinaryExpression(e1 : Expression, op: Operator, e2: Expression) extends Expression{
    override lazy val toString = s"$e1$op$e2"
    def dump( out : (String)=>Void ) = {
      out( s"Binary $op")
      e1.dump( withIndent(out) )
      e2.dump( withIndent(out) )
    }

  }
  case class UnaryExpression(op: Operator, e: Expression) extends Expression{
    override lazy val toString = s"$op$e"
    def dump( out : (String)=>Void ) = {
      out( s"Unary $op")
      e.dump( withIndent(out) )
    }
  }
  case class Number(value: String) extends Expression{
    override val toString = value
    def dump( out : (String)=>Void ) = out(toString)
  }
  case class Variable(symbol: Symbol) extends Expression{
    override val toString = symbol.name
    def dump( out : (String)=>Void ) = out(toString)
  }
}


class FormulaParser extends RegexParsers{


  import FormulaParser._

  def symbol : Parser[Symbol] = "\\p{IsAlphabetic}(\\p{IsAlphabetic}|[0-9])*".r ^^ {
    case s => Symbol(s)
  }

  def variable : Parser[Variable] = symbol ^^ { case s => Variable(s) }

  def plus : Parser[String] = "\\+".r

  def minus : Parser[String] = "-".r

  def expressionOperator : Parser[ExpressionOperator] = (plus | minus) ^^ {
    case s => ExpressionOperator(s)
  }

  def multiply : Parser[String] = "\\*".r

  def divide : Parser[String] = "/".r

  def mod : Parser[String] = "%".r

  def factorOperator : Parser[FactorOperator] = (multiply | divide | mod) ^^ {
    case s => FactorOperator(s)
  }

  def number : Parser[Number] = "[0-9]+(\\.[0-9]*)?([eE][\\+-]?[0-9]*)?".r ^^ {
    case s => Number(s)
  }

  def functionArg : Parser[Expression] = expression

  def functionCall : Parser[FunctionCall] = {
    symbol ~ "(" ~ (functionArg ~ rep( "," ~> functionArg )).? ~ ")" ^^ {
      case s ~ _ ~ Some(arg ~ args ) ~ _ => FunctionCall(s, arg :: args )
      case s ~ _ ~ None ~ _ => FunctionCall(s, Nil)  
    }
  }

  def expression : Parser[Expression] = term ~ expressionPrime ^^ {
    case term ~ ep => ep.toExpression(term)
  }

  trait EPrime{
    def toExpression(firstTerm: Expression) : Expression 
  }
  case class EP( operator: Operator, expression: Expression, expressionPrime: EPrime ) extends EPrime{
    def toExpression(firstTerm: Expression) = {
      expressionPrime.toExpression( BinaryExpression(firstTerm, operator, expression) )
    }
  }
  case object Lambda extends EPrime{
    def toExpression(firstTerm: Expression) = firstTerm
  }

  def lambda : Parser[EPrime] = "".r ^^ { case _ => Lambda }

  def expressionPrime : Parser[EPrime] = {
    expressionOperator ~ term ~ expressionPrime ^^ { case o ~ t ~ ep => EP(o,t,ep) } |
    lambda
  }

  def term : Parser[Expression] = factor ~ termPrime ^^ {
    case factor ~ tp => tp.toExpression(factor)
  }

  def termPrime : Parser[EPrime] = {
    factorOperator ~ factor ~ termPrime ^^ { case o ~ f ~ tp => EP(o,f,tp) } |
    lambda
  }

  def factor : Parser[Expression] = {
    "(" ~> expression <~ ")" |
    functionCall |
    variable |
    number |
    minus ~ expression ^^ { case o ~ e => UnaryExpression(ExpressionOperator(o),e) } |
    plus ~> expression
  }


}



object ParserMain extends App{

  import FormulaParser.Expression

  val p = new FormulaParser

  def testSymbol() = {
    val tests = Seq(
      "hola",
      "Adiós",
      "Asdf12S",
      "0adf",
      "__asdf__"
    )

    for( t <- tests ){
      val result = p.parseAll( p.symbol, t )
      println( s"$result")
    }
  }


  def testNumber(){
    val tests = Seq(
      "1",
      "0",
      "1.0",
      "1234124.123414",
      "0e10",
      "1234.1244e-10"
    )

    for( t <- tests ){
      val result = p.parseAll( p.number, t )
      println( s"$result")
    }

  }

  def testFunctionCall(){
    val tests = Seq(
      "a()",
      "b(a)",
      "b(1,2,3)",
      "hola(adios+1)",
      "hola(adios(1)+1)",
      "(b)"
    )

    for( t <- tests ){
      val result = p.parseAll( p.functionCall, t )
      println( s"$result")
    }

  }

  def testExpression(){
    val tests = Seq(
      "a",
      "a+b-c",
      "-a",
      "a*b-c*d",
      "sen(x*t+f)",
      "sin(x,cos(x*t+f),a())",
      "-52",
      "+abc"


    )

    for( t <- tests ){
      p.parseAll[Expression]( p.expression, t ) match{
        case p.Success(result, _) =>
          val r : Expression = result
          println( s"$r")
          r.dump(println)
        case p.NoSuccess(msg,_) =>
          println( "No success:" + msg )

      }
      
    }

  }

  def testEvaluation(){
    val tests = Seq(
      "a",
      "a+b-c",
      "-a",
      "a*b-c*d",
      "sen(x*t+f)",
      "avg(x,cos(x*t+f),a())",
      "-52",
      "+abc"


    )

    implicit val ec = new VariableMapExpressionContext with FunctionMapExpressionContext{
      override val variableMap: Map[String, Double] = Map(
        "a" -> 1,
        "b" -> 2,
        "c" -> 3,
        "d" -> 4,
        "t" -> 0.5,
        "f" -> 0.25,
        "x" -> -1

      )
      override val functionMap: Map[String, Seq[Double] => Double] = Map(
        "sen" -> { case a => Math.sin(a.head) },
        "cos" -> { case a => Math.cos(a.head) },
        "avg" -> { case args => args.sum/args.size },
        "a" -> { case _ => 10 }
      )
    }

    for( t <- tests ){
      p.parseAll[Expression]( p.expression, t ) match{
        case p.Success(result, _) =>
          val r : Expression = result
          val v = FormulaEvaluator.computeValue(r)
          println( s"$r : $v")
        case p.NoSuccess(msg,_) =>
          println( "No success:" + msg )

      }

    }

  }



  testEvaluation()
}
