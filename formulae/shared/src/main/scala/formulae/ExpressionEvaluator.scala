package formulae

import scala.util.{Failure, Success, Try}

object FormulaEvaluator {

  import ExpressionParser._

  trait ExpressionContext {
    def apply(v: Variable): Try[Double]

    def apply(f: FunctionCall): Try[Double]
  }

  trait VariableMapExpressionContext extends ExpressionContext {
    val variableMap: (String) => Double

    def apply(v: Variable) = Try(variableMap(v.symbol.name))
  }

  trait FunctionMapExpressionContext extends ExpressionContext {
    val functionMap: (String) => (Seq[Double]) => Double

    def apply(f: FunctionCall): Try[Double] = {
      val args = f.arguments.map(e => computeValue(e)(this))
      val firstError = args.find(_.isFailure)
      if (firstError.isDefined) {
        firstError.get
      }
      else Try {
        val fun = functionMap(f.name.name)
        fun(args.map(_.get))
      }
    }
  }

  def computeValue(e: Expression)(implicit context: ExpressionContext): Try[Double] = e match {
    case fc: FunctionCall => context(fc)
    case BinaryExpression(e1, op, e2) => op.operator match {
      case "+" => for (v1 <- computeValue(e1); v2 <- computeValue(e2)) yield v1 + v2
      case "-" => for (v1 <- computeValue(e1); v2 <- computeValue(e2)) yield v1 - v2
      case "/" => for (v1 <- computeValue(e1); v2 <- computeValue(e2)) yield v1 / v2
      case "*" => for (v1 <- computeValue(e1); v2 <- computeValue(e2)) yield v1 * v2
      case "%" => for (v1 <- computeValue(e1); v2 <- computeValue(e2)) yield v1 % v2  
      case _ => Failure(new Exception(s"Operador binario no reconocido: $op"))
    }
    case UnaryExpression(op, e) => op.operator match {
      case "+" => for (v <- computeValue(e)) yield v
      case "-" => for (v <- computeValue(e)) yield -v
      case _ => Failure(new Exception(s"Operador unario no reconocido: $op"))

    }
    case Number(value) => Success(value.toDouble)
    case v: Variable => context(v)
    case _ => Failure(new Exception(s"Expresión no reconocida: $e"))
  }
}

import FormulaEvaluator._

class DefaultExpressionContext extends ExpressionContext with
  VariableMapExpressionContext with
  FunctionMapExpressionContext {
  override val variableMap: collection.mutable.Map[String, Double] = collection.mutable.Map()

  def addVariable( pair: (String,Double) ) : scala.Unit = addVariable(pair._1, pair._2)
  def addVariable(name: String, value: Double ) : scala.Unit = variableMap += name -> value

  override val functionMap: collection.mutable.Map[String, Seq[Double] => Double] = collection.mutable.Map()

  def addFunction( pair: (String,(Seq[Double])=>Double) ) : scala.Unit = addFunction(pair._1, pair._2)
  def addFunction(name: String, function: (Seq[Double]) => Double) : scala.Unit = functionMap += name -> function
}
