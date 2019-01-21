package formulae


trait Formula{

  import Localizable._
  import Formula._

  def name: Localizable

  def description: Localizable

  def variables : Map[String, MagnitudeUnit]

  def compute( inValues : Map[String, Double] ) : Map[String, FormulaError Either Double]
}

object Formula{

  import BlockParser._


  private val formulaMap = scala.collection.mutable.Map[String,DefaultExpressionFormula]()

  def get(id: String) : Formula = internalGet(id)

  private def internalGet(id: String) = formulaMap.get(id) match{
    case Some(f) =>
      f
    case None =>
      val f = new DefaultExpressionFormula(id)
      formulaMap(id) = f
      f
  }

  case class FormulaError(msg: String, stack: Option[Throwable] = None )

  def apply(b: Block) = parseBlock(b)

  def parseBlock(b: Block) : Option[Formula] = b match{
    case Block("Formula",id,entries) =>
      val ret = internalGet(id)
      parseEntries(ret,entries)
      Some(ret)
    case _ =>
      None
  }

  def parseEntries(f: DefaultExpressionFormula, entries: Seq[Entry])  = {
    entries.foreach( _ match{
      case Entry("Variable",arg,value) =>
        f.variablesMap(arg) = Magnitude.unitFromSymbol(value).get

      case Entry("Expression",arg,value) =>
        f.expressionsMap(arg) = ExpressionParser.parse(value)

      case Entry("Name",arg,value) =>
        f.name(Locale(arg)) = value

      case e =>
        throw new RuntimeException(s"Unknown formula entry: $e" )
    })
  }




  private class DefaultExpressionFormula(val id: String) extends Formula{

    import scala.util._
    import ExpressionParser._

    val variablesMap = scala.collection.mutable.Map[String, MagnitudeUnit]()
    val expressionsMap = scala.collection.mutable.Map[String, Expression]()

    def variables = variablesMap.toMap
    def expressions = expressionsMap.toMap

    val name = Localizable()
    val description = Localizable()


    def compute( inValues: Map[String,Double] ) : Map[String, FormulaError Either Double] = {


      implicit val context = DefaultExpressionContext()
      inValues.foreach( context.addVariable )


      def tryComputeVariable( variable: String ) : FormulaError Either Double = expressionsMap.get(variable) match{
        case Some(expr) =>
          ExpressionEvaluator(expr) match{
            case Success(d) => Right(d)
            case Failure(f) => Left(FormulaError(f.getMessage(),Some(f)) )
          }
        case None =>
          Left(
            FormulaError(
              """saber variables de las expresiones
            encontrar una expresión con la variable buscada, con el resto definidas y con su resultado definido"""
            )
          )

      }

      val ret = variables.keys.map( name =>
        inValues.get(name) match{
          case Some(value) => name -> Right(value)
          case None => name -> tryComputeVariable(name)
        }
      )

      ret.toMap
    }
  }
}
