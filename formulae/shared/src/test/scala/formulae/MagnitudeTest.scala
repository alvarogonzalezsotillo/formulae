package formulae

import org.scalatest._



class MagnitudeTest extends FlatSpec {

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


  it should "convert" in {
    import MagnitudeUnit._
    import Measure._
    val v1 = Measure(10, speed.SIunit)
    val v2 = v1.to("km/h")
    assert( v2.value == 3600*10/1000 )
  }


}
