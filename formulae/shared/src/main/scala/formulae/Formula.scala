package formulae


trait Formula extends Localizable{

  import Localizable._
  import Formula._

  def names: Names

  def variables : Map[String, MagnitudeUnit]

  def compute( inValues : Map[String, Double] ) : Map[String, FormulaError Either Double]
}

object Formula{
  case class FormulaError(msg: String, stack: Option[Throwable] = None )
}


trait ExpressionFormula extends Formula{

  import ExpressionParser._

  def expressions : Map[String, Expression]

}

class DefaultExpressionFormula extends ExpressionFormula{

  import Formula._
  import scala.util._
  import ExpressionParser._

  val variablesMap = scala.collection.mutable.Map[String, MagnitudeUnit]()
  val expressionsMap = scala.collection.mutable.Map[String, Expression]()
  val localesMap = scala.collection.mutable.Map[Locale,String]()

  def variables = variablesMap.toMap
  def expressions = expressionsMap.toMap
  def names  = localesMap


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
