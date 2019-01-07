package formulae

package ecuacion

import org.scalatest._



class ExpressionParserTest extends FlatSpec {

  it should "Parse symbol" in {

    def log(s:String) = { println(s) }

    import ExpressionParser.Expression
    val p = new ExpressionParser

    val tests = Seq(
      "hola" -> true,
      "Adiós" -> false,
      "Asdf12S" -> true,
      "0adf" -> false,
      "__asdf__" -> false
    )

    for( (expr,value) <- tests ){
      p.parseAll( p.symbol, expr ) match{
        case p.Success(result, _) =>
          assert( value, s"Parse was successful, $expr shoud be $value" )
        case p.NoSuccess(msg,_) =>
          assert( !value, s"Parse wasnt successful, $expr shoud be $value: $msg")
      }

    }
  }
}
