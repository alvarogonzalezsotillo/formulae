package formulae 

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers



object FormulaParser{

  def withIndent( out : (String)=>Void, indent: String = "  " ) = (s:String) => out( indent + s )

  case class EntryValue( value: String )

  case class EntryName( name: String )

  case class EntryArg( arg: String )

  case class Entry( entryName: EntryName, entryArg: EntryArg, entryValue: EntryValue ){
    val name = entryName.name
    val value = entryValue.value
    val arg = entryArg.arg
  }

  case class Block( theType: String, name: String, entries: Seq[Entry] )
}


class FormulaParser extends RegexParsers{


  import FormulaParser._

  def endOfLine : Parser[String] = "(\\s|\n)*\n".r

  def entryName : Parser[EntryName] = "\\w+".r ^^ { case name => EntryName(name) }
  def entryArg: Parser[EntryArg] = "[\\w| ]+".r ^^ { case arg => EntryArg(arg) }
  def entrySimpleValue : Parser[EntryValue] = "[^\n]*".r ^^ { case value => EntryValue(value) }
  def tripleQuote : Parser[String] = "\"\"\"".r
  def entryMultilineValue : Parser[EntryValue] = tripleQuote ~ "([^\"]|\"[^\"]|\"\"[^\"])*".r ~ tripleQuote ^^ {
    case _ ~ v ~ _  => EntryValue(v)
  }

  def entryValue : Parser[EntryValue] = entryMultilineValue | entrySimpleValue

  def entry : Parser[Entry] = "\\s*".r ~ entryName ~ "\\s*\\(".r ~ entryArg  ~ "\\)\\s*:\\s*".r ~ entryValue  ~ endOfLine.? ^^ {
    case _ ~ name ~ _ ~ arg ~ _ ~ value ~ _ => Entry(name,arg,value)
  }

  def entries : Parser[Seq[Entry]] = rep(entry)

  def formula : Parser[Block] = "\\s*Formula\\s*\\(".r ~ entryArg ~ "\\)\\s*{".r ~ entries ~ "\\s*}" ^^ {
    case _ ~ EntryArg(name) ~ _ ~ entries ~ _ => Block( "Formula", name, entries )
  }

}
