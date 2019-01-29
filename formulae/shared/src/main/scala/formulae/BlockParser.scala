package formulae 

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.RegexParsers



object BlockParser{

  def withIndent( out : (String)=>Void, indent: String = "  " ) = (s:String) => out( indent + s )

  case class EntryValue( value: String )

  case class EntryName( name: String )

  case class EntryArg( arg: String )

  case class Entry( name: String, args: Seq[String], value: String )

  case class Block( theType: String, args: Seq[String], entries: Seq[Entry] )
}


class BlockParser extends RegexParsers{


  import BlockParser._

  def endOfLine : Parser[String] = "(\\s|\n)*\n".r

  def entryName : Parser[EntryName] = "\\w+".r ^^ { case name => EntryName(name) }
  def entryArg: Parser[EntryArg] = "[\\w| ]+".r ^^ { case arg => EntryArg(arg) }
  def entrySimpleValue : Parser[EntryValue] = "[^\n]*".r ^^ { case value => EntryValue(value) }
  def tripleQuoteBegin : Parser[String] = "<<<".r
  def tripleQuoteEnd : Parser[String] = ">>>".r
  def entryMultilineValue : Parser[EntryValue] = tripleQuoteBegin ~ "([^>]|>[^>]|>>[^>])*".r ~ tripleQuoteEnd ^^ {
    case _ ~ v ~ _  => EntryValue(v)
  }

  def entryValue : Parser[EntryValue] = entryMultilineValue | entrySimpleValue

  def argsList : Parser[Seq[EntryArg]] = """\s*\(""".r ~ repsep(entryArg,"""\s*,\s*""".r) ~ """\s*\)""".r ^^ {
    case _ ~ seq ~ _ => seq
  }

  def entry : Parser[Entry] = "\\s*".r ~ entryName ~ argsList.? ~ "\\s*:\\s*".r ~ entryValue  ~ endOfLine.? ^^ {
    case _ ~ name ~ Some(args) ~ _ ~ value ~ _ => Entry(name.name,args.map(_.arg),value.value)
    case _ ~ name ~ None ~ _ ~ value ~ _ => Entry(name.name,Seq(),value.value)
  }

  def entries : Parser[Seq[Entry]] = rep(entry)

  def block : Parser[Block] = "\\s*".r ~ entryName ~ argsList ~ "\\{".r ~ entries ~ "\\s*\\}".r ^^ {
    case _ ~ EntryName(block) ~ names ~ _ ~ entries ~ _ => Block( block, names.map(_.arg), entries )
  }

}
