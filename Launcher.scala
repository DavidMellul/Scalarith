package Exercise_5

import scala.io.StdIn

/**
  * Created by David on 27/03/2017.
  */
object Launcher {
  def main(args: Array[String]): Unit = {
    while (true) {
      var expression = StdIn.readLine(">")
      if( List("quit","stop","bye","end") contains expression ) { println("Thank you :) Bye."); System.exit(0) }
      try {
        println(LangageInterpreter(expression).compute())
      }
      catch {
        case ex: Exception => println("Please input a correct expression.")
      }
    }
  }
}
