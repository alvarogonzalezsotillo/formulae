package formulae

import org.scalatest._



class FormulaParserTest extends FlatSpec {

  it should "Parse entry" in {

    def log(s:String) = { println(s) }

    import FormulaParser._
    val p = new FormulaParser

    val tests = Seq(
      "hola(a):b" -> true,
      "hola  ( a ) : b" -> true,
      "hola : b" -> false,
      "hola:b" -> false,
      "hola(a())" -> false,
      "entry(arg):value\n" -> true,
      "entry(arg):value" -> true,
      "entry(arg with spaces): value with spaces" -> true,
      "entry(arg with spaces): value with spaces\n\n" -> true,
      "entry(arg with spaces): \"\"\"  long value    \"\"\"" -> true,
      "entry(arg with spaces): \"\"\"  long  \n value    \"\"\"" -> true
      
   )

    for( (expr,value) <- tests ){
      p.parseAll( p.entry, expr ) match{
        case p.Success(result, _) =>
          assert( value, s"Parse was successful, $expr shoud be $value" )
        case p.NoSuccess(msg,_) =>
          assert( !value, s"Parse wasnt successful, $expr shoud be $value: $msg")
      }

    }
  }

  it should "Parse multiline value" in {

    def log(s:String) = { println(s) }

    import FormulaParser._
    val p = new FormulaParser

    val tests = Seq(
      "sdfasdf\n" -> false,
      "\"\"\"valid\"\"\"\n\n" -> true,
      "\"\"\"valid\"\"\"" -> true,
      "\"\"\"invalid\"\"" -> false,
      "\"\"\"invalid\"\"\n\n" -> false,
      "\"\"\"valid with \n newline\"\"\"\n\n" -> true

    )

    for( (expr,value) <- tests ){
      p.parseAll( p.entryMultilineValue, expr ) match{
        case p.Success(result, _) =>
          assert( value, s"Parse was successful, $expr shoud be $value" )
        case p.NoSuccess(msg,_) =>
          assert( !value, s"Parse wasnt successful, $expr shoud be $value: $msg")
      }

    }
  }
}