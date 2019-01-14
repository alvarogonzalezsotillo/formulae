package formulae





trait MagnitudeUnit extends Localizable{
  val names: Localizable.Names
  val symbol : String
  val magnitude: Magnitude
  val toSI : Double
}

object MagnitudeUnit{
  private val registry = scala.collection.mutable.Map[String,MagnitudeUnit]()
  def register(u: MagnitudeUnit){
    if( registry.contains(u.symbol) ){
      ???
    }
    registry.put( u.symbol, u )
  }

  implicit def fromSymbol(symbol: String) = registry(symbol)
}

case class MUnit(
  val names: Localizable.Names,
  val symbol: String,
  val magnitude: Magnitude,
  val toSI: Double ) extends MagnitudeUnit{
  MagnitudeUnit.register(this)
}

class SIUnit(
  override val names: Localizable.Names,
  override val symbol: String,
  override val magnitude: Magnitude ) extends MUnit(names, symbol, magnitude, 1.0)

trait Magnitude extends Localizable{
  import Magnitude._
  val names: Localizable.Names
  val symbol : Symbol
  val units : Seq[MagnitudeUnit]
  val SIunit : MagnitudeUnit
}

object Magnitude{
  type Symbol = String
}


object speed extends Magnitude{
  import Magnitude._
  import Locale._
  import Localizable._

  val names : Names = "speed"
  val symbol = "v"
  val SIunit = new SIUnit( "meters per second", "m/s", this)
  val units = Seq(
    SIunit,
    MUnit("km per hour", "km/h", this, 1000.0/3600.0 )
  )
}


class Measure( val value: Double, val unit: MagnitudeUnit ){
  def to( u: MagnitudeUnit ) = {
    if( u.magnitude == unit.magnitude ){
      val v = value * unit.toSI / u.toSI;
      Some(Measure(v,u))
    }
    else{
      None
    }
  }

  def toSI = to( unit.magnitude.SIunit )
  override val toString = s"$value ${unit.symbol}"

}

object Measure{
  implicit def toMeasure( o: Option[Measure] ) : Measure = o.get
  def apply( value: Double, unit: MagnitudeUnit ) = new Measure(value,unit)
  def apply( value: Double, magnitude: Magnitude) = new Measure(value, magnitude.SIunit )
}

trait FormulaData extends Localizable{
  import FormulaData._
  val variables : Seq[Variable]
  val names : Map[Locale,String]
  val computations : Map[Variable,Computation]
}

object FormulaData{
  type VariableName = String
  type Variable = (VariableName,Magnitude.Symbol)
  type Computation = String
}


object MagnitudeMain{


  def testMeasure() = {
    import MagnitudeUnit._
    import Measure._
    val v1 = Measure(10, speed.SIunit)
    val v2 = v1.to("km/h")
    println( s"$v1 -> $v2")
  }

  def testIString(){

    import Locale._

    implicit val locale = Locale("en")

    implicit val lm : iString.LocaleMap = Map(

      Locale("es") -> Map(
        "a" -> "a en español",
        "b" -> "b en español"
      ),
      Locale("en") -> Map(
        "a" -> "a en inglés",
        "b" -> "b en inglés"
      )

    )

    val lable = new Localizable{
      val names = Map[Locale,String](
        ("es", "en español"),
        ("en", "en inglés")
      )
    }

    def printLocalized(is: iString) = println( s"$is" )


    printLocalized(lable)
    printLocalized("b")
    printLocalized(speed)
    speed.units.foreach( u => printLocalized(u) )

  }

  testIString()
}
