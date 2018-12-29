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
      val s = o.toString
      new iString( localeMap(locale)(s) )
  }

  implicit def toiString(o: Any)(implicit locale: Locale, localeMap: LocaleMap ) = apply(o)
}
