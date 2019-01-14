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

  

  def entryName : Parser[EntryName] = "\\w+".r ^^ { case name => EntryName(name) }
  def entryArg: Parser[EntryArg] = "[\\w| ]+".r ^^ { case arg => EntryArg(arg) }
  def entrySimpleValue : Parser[EntryValue] = "[^\n]*".r ^^ { case value => EntryValue(value) }
  def tripleQuote : Parser[String] = "\"\"\"".r
  def entryMultilineValue : Parser[EntryValue] = tripleQuote ~ "(.|\n)*" ~ tripleQuote ~ "\\s*\n" ^^ {
    case _ ~ v ~ _ ~ _ => EntryValue(v)
  }

  def entryValue : Parser[EntryValue] = entrySimpleValue | entryMultilineValue

  def entry : Parser[Entry] = "\\s*".r ~ entryName ~ "\\s*\\(".r ~ entryArg  ~ "\\s*\\)\\s*:\\s*".r ~ entryValue ^^ {
    case _ ~ name ~ _ ~ arg ~ _ ~ value => Entry(name,arg,value)
  }

  def entries : Parser[Seq[Entry]] = rep(entry)

  def formula : Parser[Block] = "\\s*Formula\\s*\\(".r ~ entryArg ~ "\\)\\s*{".r ~ entries ~ "\\s*}" ^^ {
    case _ ~ EntryArg(name) ~ _ ~ entries ~ _ => Block( "Formula", name, entries )
  }

}
