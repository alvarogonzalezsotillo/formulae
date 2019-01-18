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



