package formulae

import org.scalatest._



class ExpressionEvaluatorTest extends FlatSpec {

  it should "Eval expressions" in {

    def log(s:String) = {
      //println(s)
    }


    implicit val context = DefaultExpressionContext()

    Seq[(String,Double)](
      "a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4,
      "t" -> 0.5,
      "f" -> 0.25,
      "x" -> 0.125
    ).map(context.addVariable)

    import Math._

    Seq[(String,(Seq[Double])=> Double)](
      "a" -> { case(a) => 10 }
    ).map(context.addFunction)

    val tests = Seq[(String,Double)](
      "a" -> 1,
      "a+b-c" -> 0,
      "-a" -> -1,
      "a*b-c*d" -> (1*2-3*4),
      "sen(x*t+f)" -> sin(0.125*0.5+0.25),
      "cos(x*t+f)" -> cos(0.125*0.5+0.25),
      "sen(x*t+f)*sen(x*t+f) + cos(x*t+f)*cos(x*t+f)" -> (sin(0.125*0.5+0.25)*sin(0.125*0.5+0.25) + cos(0.125*0.5+0.25)*cos(0.125*0.5+0.25)),
      "avg(x,cos(x*t+f),a())" -> (0.125 + cos(0.125*0.5+0.25) + 10)/3,
      "-52" -> -52,
      "11%d" -> 11%4,
      "sqrt(16)" -> 4
    )

    import ExpressionParser.Expression
    val p = new ExpressionParser

    for( (expr,value) <- tests ){
      p.parseAll[Expression]( p.expression, expr ) match{
        case p.Success(result, _) =>
          val r : Expression = result
          val v = ExpressionEvaluator.computeValue(r)
          log( s"$expr: $r : $v")
          assert( v.get == value)
        case p.NoSuccess(msg,_) =>
          fail( "No success:" + msg )
      }

    }
  }
}
