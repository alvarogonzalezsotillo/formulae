package formulae 

import formulae.ExpressionEvaluator.{FunctionMapExpressionContext, VariableMapExpressionContext}
import formulae.ExpressionParser.{BinaryExpression, Expression, FunctionCall, Number, UnaryExpression, Variable}

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers
import scala.{Unit => Void}



object ExpressionParser{


  def withIndent( out : (String)=>Void, indent: String = "  " ) = (s:String) => out( indent + s )

  trait Expression{
    def dump( out : (String) => Void = println ) : Void
  }

  case class Symbol(name:String){
    override val toString = name
  }
  case class Operator(operator: String){
    override val toString = operator
  }

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


  def parse(s:String) = {
    val p = new ExpressionParser()

    p.parseAll( p.expression, s ) match{
      case p.Success(result, _) =>
        result
      case p.NoSuccess(msg,_) =>
        throw new RuntimeException(msg)
    }
  }
}


class ExpressionParser extends RegexParsers{


  import ExpressionParser._

  def symbol : Parser[Symbol] = "[a-zA-Z]([a-zA-Z]|[0-9])*".r ^^ {
    case s => Symbol(s)
  }

  def variable : Parser[Variable] = symbol ^^ { case s => Variable(s) }

  def plus : Parser[String] = "\\+".r

  def minus : Parser[String] = "-".r

  def expressionOperator : Parser[Operator] = (plus | minus) ^^ {
    case s => Operator(s)
  }

  def multiply : Parser[String] = "\\*".r

  def divide : Parser[String] = "/".r

  def mod : Parser[String] = "%".r

  def factorOperator : Parser[Operator] = (multiply | divide | mod) ^^ {
    case s => Operator(s)
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
    minus ~ expression ^^ { case o ~ e => UnaryExpression(Operator(o),e) } |
    plus ~> expression
  }


}



object ParserMain extends App{

  import ExpressionParser.Expression

  val p = new ExpressionParser

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

  
  



  }
