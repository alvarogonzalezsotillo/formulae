package formulae


class Locale(val name: String){
  override def equals(o : Any) = o match {
    case l: Locale => l.name == name
    case _ => false  
  }
  override lazy val toString = s"Locale($name)"
}

object Locale{
  implicit def toLocale( s: String ) = Locale(s);
  def apply(s:String) = new Locale(s)
}

trait Localizable{
  import Localizable._

  def names : Names
}

object Localizable{
  type Names = (Locale) => String
  implicit def forEnglish(s:String) : Names = {
    Map( Locale("en") -> s )
  }
}

class iString(val iMsg: String){
  override val toString = iMsg
}

object iString{

  type LocaleMap = (Locale) => (String) => String

  def apply(o : Any)(implicit locale: Locale, localeMap: LocaleMap ) = o match{
    case l: Localizable =>
      new iString(l.names(locale) )
    case o =>
      import scala.util._
      val s = o.toString
      Try{
        new iString( localeMap(locale)(s) )
      } match{
        case Success(is) => is
        case Failure(_) => new iString(s)
      }
  }

  implicit def toiString(o: Any)(implicit locale: Locale, localeMap: LocaleMap ) = apply(o)
}

class DefaultLocaleMap extends iString.LocaleMap {

  type MutableMap[K,V] = scala.collection.mutable.HashMap[K,V]

  private val map = new MutableMap[Locale,(String)=>String]()

  private class ReadOnlyMap( m: MutableMap[String,String]) extends ((String)=>String) {
    def apply(s:String) = m(s)
    def originalMap = m
  }

  def add(key: String, value: String)(implicit locale: Locale ) = {
    apply(locale).asInstanceOf[ReadOnlyMap].originalMap(key) = value
  }

  def update(key: String, value: String)(implicit locale: Locale ) = add(key,value)

  def apply( locale: Locale ) = map.get(locale) match{
    case Some(m) =>
      m
    case None =>
      val m = new MutableMap[String,String]()
      val rom = new ReadOnlyMap(m)
      map(locale) = rom
      rom
  }
}
