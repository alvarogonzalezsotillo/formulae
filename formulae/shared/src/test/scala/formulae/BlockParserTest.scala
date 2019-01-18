package formulae

import org.scalatest._



class BlockParserTest extends FlatSpec {

  it should "parse }" in{
    assert( "\\}".r.findFirstMatchIn("}").isDefined )
    assert( "\\s*\\}".r.findFirstMatchIn("  }").isDefined )
    assert( "\\s*\\}".r.findFirstMatchIn("}").isDefined )
  }


  it should "Parse block" in {

    def log(s:String) = { println(s) }

    import BlockParser._
    val p = new BlockParser

    val tests = Seq(
      """ Block(a block) {
            entrada(arg): value
            entrada2(adsf): value
          }
      """ -> true,

      "Block ( sadf ) { otro(a): b \n  entrada( arg ) : \"\"\"  algo largo  \"\"\" \n algomas(mas):mas \n } \n" -> true

    )

    for( (expr,value) <- tests ){
      p.parseAll( p.block, expr ) match{
        case p.Success(result, _) =>
          assert( value, s"Parse was successful, $expr shoud be $value" )
        case p.NoSuccess(msg,_) =>
          assert( !value, s"Parse wasnt successful, $expr shoud be $value: $msg")
      }

    }
  }

  it should "Parse entry" in {

    def log(s:String) = { println(s) }

    import BlockParser._
    val p = new BlockParser

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

    import BlockParser._
    val p = new BlockParser

    val tests = Seq(
      "sdfasdf\n" -> false,
      "\"\"\"valid\"\"\"\n\n" -> true,
      "\"\"\"valid\"\"\"" -> true,
      "\"\"\"invalid\"\"" -> false,
      "\"\"\"invalid\"\"\n\n" -> false,
      "\"\"\"valid with \n newline\"\"\"\n\n" -> true,
      "\"\"\"valid with \n newline and \"quotes\" \"\"\"\n\n" -> true
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
