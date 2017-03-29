package Exercise_5


import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by David on 27/03/2017.
  */


object LangageInterpreter {
  def apply(s: String): LangageInterpreter = new LangageInterpreter(s)
}

class LangageInterpreter(s: String) extends JavaTokenParsers {

  def expr: Parser[Double] = factor ~ rep("+" ~ factor | "-" ~ factor) ^^ {
    case o1 ~ o2 => o2.foldLeft(o1) {
      case (a, "+" ~ b) => a + b
      case (a, "-" ~ b) => a - b
    }
  }

  def factor: Parser[Double] = power ~ rep("*" ~ power | "/" ~ power) ^^ {
    case o1 ~ o2 => o2.foldLeft(o1) {
      case (a, "*" ~ b) => a * b
      case (a, "/" ~ b) => a / b
    }
  }

  def power: Parser[Double] = operand ~ rep("^" ~ operand) ^^ {
    case o1 ~ o2 => o2.foldLeft(o1) {
      case (a, "^" ~ b) => Math.pow(a, b)
    }
  }

  def operand: Parser[Double] =  factorial | percent | number | expression | exponential | ln | E | trigonometry | sqrt |  pi

  def number: Parser[Double] = floatingPointNumber ^^ { f => f toDouble }

  def expression: Parser[Double] = "(" ~> expr <~ ")" ^^ { f => f }

  def E: Parser[Double] = "E" ^^ { f => 10 }

  def exponential: Parser[Double] = "e" ^^ { f => Math.exp(1) }

  def ln: Parser[Double] = "ln(" ~ operand ~ ")" ^^ {
    case o1 ~ o ~ o2 => {
      if(o > 0) Math.log(o) else { println("Not defined"); 0 }
    }
  }

  def factorial : Parser[Double] = (number | expression) ~ "!" ^^ {
    case o ~ o1 =>  o!
  }

  def sqrt : Parser[Double] = "sqrt(" ~ operand ~ ")" ^^ {
    case o1 ~ o ~ o2 => Math.sqrt(o)
  }

  def percent: Parser[Double] = (number | expression) ~ ("% of" | "percents of") ~ (number | expression) ^^ {
    case o1 ~ o ~ o2 => (o1 * o2)/100.0
  }

  def trigonometry: Parser[Double] = ("sin(" | "cos(" | "tan(" | "asin(" | "acos(" | "atan(") ~ expr ~ ")" ^^ {
    case o1 ~ o ~ o2 => o1 match {
      case ("cos(") => Math.cos(o)
      case ("sin(") => Math.sin(o)
      case ("tan(") => Math.tan(o)
      case ("acos(") => Math.acos(o)
      case ("asin(") => Math.asin(o)
      case ("atan(") => Math.atan(o)
    }
  }

  def pi: Parser[Double] = "pi" ^^ { f => Math.PI }


  implicit class int2Factorial(n: Double) {
    def ! : Double = {
      var f: BigInt = 1
      for(i <- BigInt(2) to Math.abs(n.toInt)) f *= i
      f toDouble
    }
  }

  def compute(): Double = {
    parseAll(expr, s) get
  }
}

