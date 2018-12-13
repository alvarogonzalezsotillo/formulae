import scala.util.parsing.combinator.RegexParsers


class Parser extends RegexParsers{

  case class Symbol(name:String)
  case class Function(name: String)
  case class Plus()
  case class Minus()
  case class Number(value: String)


  def symbol : Parser[Symbol] = "\\p{IsAlphabetic}(\\p{IsAlphabetic}|[0-9])*".r ^^ {
    case s => Symbol(s)
  }

  def plus : Parser[Plus] = "\\+".r ^^ { case _ => Plus() }

  def minus : Parser[Minus] = "-".r ^^ { case _ => Minus() }

  def number : Parser[Number] = "[0-9]+(\\.[0-9]*)?([eE][\\+-]?[0-9]*)?".r ^^ {
    case s => Number(s)
  }

  def sum = symbol ~ rep( plus ~ symbol )
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
      val result = p.parse( p.symbol, t )
      println( s"$result")
    }
  }

  def testSum(){
    val tests = Seq(
      "hola",
      "Adiós + abc",
      "Asdf12S + abc + edf",
      "aa0df+ hadfuf +  j12343",
    )

    for( t <- tests ){
      val result = p.parse( p.sum, t )
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
      val result = p.parse( p.number, t )
      println( s"$result")
    }

  }


  testNumber()
}
