package formulae

import scala.util.{Failure, Success, Try}

object FormulaEvaluator {

  import FormulaParser._

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

object DefaultExpressionContext extends ExpressionContext with
  VariableMapExpressionContext with
  FunctionMapExpressionContext {
  override val variableMap: Map[String, Double] = Map(
    "a" -> 1,
    "b" -> 2,
    "c" -> 3,
    "d" -> 4,
    "t" -> 0.5,
    "f" -> 0.25,
    "x" -> -1

  )

  def checkAnyArgs(f: String, args: Seq[Double], value: => Double) = {
    if (args.size > 0) {
      value
    }
    else {
      throw new RuntimeException(s"Bad number of arguments $f")
    }
  }

  def checkArgs(f: String, got: Int, expected: Int, value: => Double) = {
    if (got != expected)
      throw new RuntimeException(s"bad number of arguments $f: got:$got expected:$expected")
    else
      value
  }

  override val functionMap: Map[String, Seq[Double] => Double] = Map(
    "sen" -> { case a => checkArgs("sen", a.size, 1, Math.sin(a.head)) },
    "cos" -> { case a => checkArgs("cos", a.size, 1, Math.cos(a.head)) },
    "avg" -> { case args => checkAnyArgs("avg", args, args.sum / args.size) },
    "a" -> { case args => checkArgs("a", args.size, 0, 10) }
  )
}
