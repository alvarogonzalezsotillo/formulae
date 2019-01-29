package formulae



class Registry[K,V]( factory: (K)=>V ){
  private val registry = scala.collection.mutable.Map[K,V]()

  def apply(k:K) = registry.get(k) match{
    case Some(v) =>
      v
    case None =>
      val ret = factory(k)
      registry(k) = ret
      ret
  }

  def keys = registry.keys
  def values = registry.values
}


trait MagnitudeUnit{
  import ExpressionParser._

  val name: Localizable
  val symbol : String
  def magnitude: Magnitude
  def toSI : Expression
  def fromSI : Expression
}

object MagnitudeUnit{
  private val registry = scala.collection.mutable.Map[String,MagnitudeUnit]()
}


trait Magnitude{
  import Magnitude._
  val name: Localizable
  val symbol : Symbol
  def units : Seq[MagnitudeUnit]
  def SIunit : MagnitudeUnit
}

object Magnitude{
  type Symbol = String

  private val registry = new Registry[Symbol,DefaultMagnitude]( new DefaultMagnitude(_) )

  private class DefaultUnit(val symbol: Symbol) extends MagnitudeUnit{
    val name = Localizable()
    var magnitude = null
    var toSI = null
    var fromSI = null
  }

  private class DefaultMagnitude(val symbol: Symbol) extends Magnitude{
    val name = Localizable()


    private val uregistry = new Registry[Symbol,DefaultUnit]( new DefaultUnit(_) )

    private def getUnit(s:Symbol) = uregistry(s)

    def units : Seq[MagnitudeUnit]= uregistry.values.toSeq

    def SIunit = units.find( _.toSI == 1 ).get 
  }

  def getMagnitude(s:Symbol) : Magnitude = registry(s)

  def magnitudes : Seq[Magnitude] = registry.values.toSeq

  def unitFromSymbol(s:Symbol) = registry.values.map(_.units).flatten.find(_.symbol==s)




}




class Measure( val value: Double, val unit: MagnitudeUnit ){
  def to( u: MagnitudeUnit ) = {
    if( u.magnitude == unit.magnitude ){
      implicit val context = DefaultExpressionContext()
      context.addVariable("value", value)
      val si = ExpressionEvaluator.computeValue(unit.toSI)
      context.addVariable("value", si.get )
      val v = ExpressionEvaluator.computeValue(u.fromSI)
      Some(Measure(v.get,u))
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



