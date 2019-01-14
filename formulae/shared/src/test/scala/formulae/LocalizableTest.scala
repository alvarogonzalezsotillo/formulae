package formulae

import org.scalatest._



class LocalizableTest extends FlatSpec {

  it should "Use default english localizable" in {

    import iString._
    import Localizable._

    implicit val lm = new DefaultLocaleMap()
    implicit val l = Locale("en")

    val localizable = new Localizable{
      val names : Names = "Hello"
    }

    val is : iString = localizable

    assert( is.toString == "Hello" )
  }

  it should "Use locale map" in {
    import iString._
    import Localizable._

    implicit val lm = new DefaultLocaleMap()
    implicit val l = Locale("en")

    lm("Hola") = "Hello"
    lm("Adiós") = "Bye"

    val is : iString = "Hola"

    assert( is.toString == "Hello" )
  }

  it should "Fallback to default" in {
    import iString._
    import Localizable._

    implicit val lm = new DefaultLocaleMap()
    implicit val l = Locale("en")

    lm("Hola") = "Hello"
    lm("Adiós") = "Bye"

    val is : iString = "unknown"

    assert( is.toString == "unknown" )
  }
  
}
