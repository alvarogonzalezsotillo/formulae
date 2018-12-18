import scala.util.parsing.combinator.RegexParsers



class Parser extends RegexParsers{

  object Parser{

    trait ExpressionContext{
      def valueOf(v: Variable)
      def valueOf(f: FunctionCall)
    }

    trait ComputedValue
    case class NumericComputedValue(value: Double)
    case class NonComputedValue(str: String)

    trait Expression{
      def computeValue(implicit context: ExpressionContext) : ComputedValue = ???
    }

    case class Symbol(name:String)
    trait Operator{
      val operator: String
    }
    case class ExpressionOperator(override val operator: String) extends Operator
    case class FactorOperator(override val operator: String) extends Operator

    case class FunctionCall(name: Symbol, arguments: Seq[Expression] ) extends Expression
    case class BinaryExpression(e1 : Expression, op: Operator, e2: Expression) extends Expression
    case class UnaryExpression(op: Operator, e: Expression) extends Expression
    case class Number(value: String) extends Expression
    case class Variable(symbol: Symbol) extends Expression
  }


  import Parser._

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



object Main extends App{

  val p = new Parser

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
      val result = p.parseAll( p.expression, t )
      println( s"$result")
    }

  }

  


  testExpression()
}
