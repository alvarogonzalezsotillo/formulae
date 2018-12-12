



case class Locale(val name: String)



trait Localizable{
  def names : (Locale) => String
}

class iString(val iMsg: String){
  override lazy val toString = iMsg
}

object iString{

  type LocaleMap = (Locale) => (String) => String

  def apply(s: String)(implicit locale: Locale, localeMaps: LocaleMap ) = {
    val a = localeMaps(locale)
    val b = a(s)
    new iString( b )
  }

  implicit def toiString(s: String)(implicit locale: Locale, localeMaps: (Locale) => (String) => String ) = apply(s)

  def apply(l : Localizable)(implicit locale: Locale ) = {
    new iString( l.names(locale) )
  }
  implicit def toiString(l: Localizable)(implicit locale: Locale ) = apply(l)
}


trait MagnitudeUnit{
  val name: String
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

  implicit def toMagnitudeUnit(symbol: String) = registry(symbol)
}

case class Unit(
  val name: String,
  val symbol: String,
  val magnitude: Magnitude,
  val toSI: Double ) extends MagnitudeUnit{
  MagnitudeUnit.register(this)
}

class SIUnit(
  override val name: String,
  override val symbol: String,
  override val magnitude: Magnitude ) extends Unit(name, symbol, magnitude, 1.0)

trait Magnitude{
  import Magnitude._
  val name: Name
  val symbol : Symbol
  val units : Seq[MagnitudeUnit]
  val SIunit : MagnitudeUnit
  override def toString = name
}

object Magnitude{
  type Name = String
  type Symbol = String
}


object speed extends Magnitude{
  val name = "speed"
  val symbol = "v"
  val SIunit = new SIUnit( "meters per second", "m/s", this)
  val units = Seq(
    SIunit,
    Unit("km per hour", "km/h", this, 1000.0/3600.0 )
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
}

trait FormulaData{
  type VariableName = String
  type Variable = (VariableName,Magnitude.Symbol)
  type Computation = String

  val variables : Seq[Variable]
  val names : Map[Locale,iString]
  val name : String
  val computations : Map[Variable,Computation]

}

object Main extends App{


  def testMeasure() = {
    import MagnitudeUnit._
    import Measure._
    val v1 = Measure(10, speed.SIunit)
    val v2 = v1.to("km/h")
    println( s"$v1 -> $v2")
  }

  def testIString(){

    implicit def toLocale( s: String ) = Locale(s);

    implicit val locale = Locale("es")
    val lable = new Localizable{
      val names = Map[Locale,String](
        ("es", "en español"),
        ("en", "en inglés")
      )
    }

    val i : iString = lable

    println( s"$i")

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

    println( lm )

    val i2 : iString = "b"

    println(s"$i2")

  }

  testIString()
}
